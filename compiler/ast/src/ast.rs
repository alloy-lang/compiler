use alloy_syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
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
        #[derive(Clone)]
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

fn first_ident(node: &impl AstElement) -> Option<String> {
    node.syntax()
        .into_node()?
        .children_with_tokens()
        .filter_map(SyntaxElement::into_token)
        .find(|token| matches!(token.kind(), SyntaxKind::Ident | SyntaxKind::OpIdent))
        .map(|token| token.text().into())
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
