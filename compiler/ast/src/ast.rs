use alloy_syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxNodePointer, SyntaxToken};
use std::fmt;
use text_size::TextRange;

macro_rules! ast_token {
    ($kind:ident, fields: [$($field:ident),*]) => {
        pub struct $kind(SyntaxToken);

        impl AstElement for $kind {
            fn cast<E: Into<SyntaxElement>>(element: E) -> Option<Self> {
                let element = element.into();
                if element.kind() == SyntaxKind::$kind {
                    Some(Self(element.into_token()?))
                } else {
                    None
                }
            }

            fn syntax(&self) -> SyntaxElement {
                self.0.clone().into()
            }
        }

        impl fmt::Debug for $kind {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct(stringify!($kind))
                    $( .field(stringify!($field), &self.$field()) )*
                    .finish()
            }
        }
    };
}

macro_rules! ast_node {
    ($kind:ident, fields: [$($field:ident),*]) => {
        #[derive(PartialEq, Eq)]
        pub struct $kind(SyntaxNode);

        impl AstElement for $kind {
            fn cast<E: Into<SyntaxElement>>(element: E) -> Option<Self> {
                let element = element.into();
                if element.kind() == SyntaxKind::$kind {
                    Some(Self(element.into_node()?))
                } else {
                    None
                }
            }

            fn syntax(&self) -> SyntaxElement {
                self.0.clone().into()
            }
        }

        impl fmt::Debug for $kind {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct(stringify!($kind))
                    $( .field(stringify!($field), &self.$field()) )*
                    .finish()
            }
        }
    };
}

macro_rules! ast_union_node {
	($name:ident, kinds: [$($kind:ident),+]) => {
		#[derive(Debug)]
		pub enum $name {
			$( $kind($kind), )+
		}

		impl AstElement for $name {
			fn cast<E: Into<SyntaxElement>>(element: E) -> Option<Self> {
                let element = element.into();
				match element.kind() {
					$( SyntaxKind::$kind => Some(Self::$kind($kind::cast(element)?)), )+
					_ => None
				}
			}

			fn syntax(&self) -> SyntaxElement {
				match self {
					$( Self::$kind(s) => s.syntax(), )+
				}
			}
		}
	};

	($name:ident, kinds: [$($kind:ident),+], $else:ident) => {
		#[derive(Debug)]
		pub enum $name {
               $else($else),
			$( $kind($kind), )+
		}

		impl AstElement for $name {
			fn cast<E: Into<SyntaxElement>>(element: E) -> Option<Self> {
                let element = element.into();
				match element.kind() {
					$( SyntaxKind::$kind => Some(Self::$kind($kind::cast(element)?)), )+
					_ => Some(Self::$else($else::cast(element)?)),
				}
			}

			fn syntax(&self) -> SyntaxElement {
				match self {
					   Self::$else(s) => s.syntax(),
					$( Self::$kind(s) => s.syntax(), )+
				}
			}
		}
	};
}

ast_token!(NilIdentifier, fields: []);

pub use expression::*;
mod expression;

pub use import::*;
mod import;

pub use module::*;
mod module;

pub use pattern::*;
mod pattern;

pub use source_file::*;
mod source_file;

pub use r#trait::*;
mod r#trait;

pub use r#type::*;
mod r#type;

pub use type_annotation::*;
mod type_annotation;

pub use value::*;
mod value;

pub use type_definition::*;
mod type_definition;

pub use behavior::*;
mod behavior;

pub use type_variable::*;
mod type_variable;

pub use path::*;
mod path;

#[allow(clippy::module_name_repetitions)]
pub trait AstElement: Sized {
    fn cast<E: Into<SyntaxElement>>(element: E) -> Option<Self>;

    fn syntax(&self) -> SyntaxElement;

    fn text(&self) -> String {
        match self.syntax() {
            SyntaxElement::Node(n) => n.text().to_string(),
            SyntaxElement::Token(t) => t.text().to_string(),
        }
    }

    fn range(&self) -> TextRange {
        self.syntax().text_range()
    }
}

#[derive(Debug)]
pub struct AstElementPointer<E: AstElement> {
    raw: SyntaxNodePointer,
    _ty: std::marker::PhantomData<E>,
}

impl<E: AstElement> AstElementPointer<E> {
    pub fn new(node: E) -> Self {
        Self {
            raw: SyntaxNodePointer::new(&node.syntax().into_node().expect("expected Syntax Node")),
            _ty: std::marker::PhantomData,
        }
    }
}

impl<E: AstElement> Eq for AstElementPointer<E> {}

impl<E: AstElement> PartialEq for AstElementPointer<E> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<E: AstElement> std::hash::Hash for AstElementPointer<E> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let AstElementPointer { raw, _ty } = self;
        raw.hash(state);
    }
}

impl<E: AstElement> Clone for AstElementPointer<E> {
    fn clone(&self) -> Self {
        Self {
            raw: self.raw.clone(),
            _ty: self._ty,
        }
    }
}

fn first_ident(node: &impl AstElement) -> Option<Ident> {
    node.syntax()
        .into_node()?
        .children_with_tokens()
        .filter_map(SyntaxElement::into_token)
        .find(|token| matches!(token.kind(), SyntaxKind::Ident))
        .and_then(Ident::cast)
}

fn first_ident_or_op_ident(node: &impl AstElement) -> Option<IdentOrOp> {
    node.syntax()
        .into_node()?
        .children_with_tokens()
        .filter_map(SyntaxElement::into_token)
        .find(|token| matches!(token.kind(), SyntaxKind::Ident | SyntaxKind::OpIdent))
        .and_then(IdentOrOp::cast)
}

fn first_child<Parent: AstElement, Child: AstElement>(parent: &Parent) -> Option<Child> {
    parent
        .syntax()
        .into_node()?
        .children_with_tokens()
        .find_map(Child::cast)
}

fn first_matching_child<Parent: AstElement, Child: AstElement>(
    parent: &Parent,
    st: SyntaxKind,
) -> Option<Child> {
    parent
        .syntax()
        .into_node()?
        .children()
        .find(|node| node.kind() == st)?
        .children_with_tokens()
        .find_map(Child::cast)
}

fn children<Parent: AstElement, Child: AstElement>(parent: &Parent) -> Vec<Child> {
    let node = parent.syntax().into_node();

    let Some(node) = node else { return Vec::new() };

    node.children_with_tokens()
        .filter_map(Child::cast)
        .collect()
}

fn all_matching_children<Parent: AstElement, Child: AstElement>(
    parent: &Parent,
    st: SyntaxKind,
) -> Vec<Child> {
    let node = parent.syntax().into_node();

    let Some(node) = node else {
        return Vec::new();
    };

    node.children()
        .filter(move |node| node.kind() == st)
        .flat_map(|node| node.children_with_tokens().filter_map(Child::cast))
        .collect::<Vec<_>>()
}
