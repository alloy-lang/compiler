use alloy_syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
use std::fmt;
use text_size::TextRange;

macro_rules! ast_token {
    ($kind:ident, fields: [$($field:ident),*]) => {
        pub struct $kind(SyntaxToken);

        impl AstToken for $kind {
            fn cast(node: SyntaxToken) -> Option<Self> {
                if node.kind() == SyntaxKind::$kind {
                    Some(Self(node))
                } else {
                    None
                }
            }

            fn syntax(&self) -> SyntaxToken {
                self.0.clone()
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
        pub struct $kind(SyntaxNode);

        impl AstNode for $kind {
            fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == SyntaxKind::$kind {
                    Some(Self(node))
                } else {
                    None
                }
            }

            fn syntax(&self) -> SyntaxNode {
                self.0.clone()
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

		impl AstNode for $name {
			fn cast(node: SyntaxNode) -> Option<Self> {
				match node.kind() {
					$( SyntaxKind::$kind => Some(Self::$kind($kind::cast(node)?)), )+
					_ => None
				}
			}

			fn syntax(&self) -> SyntaxNode {
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

		impl AstNode for $name {
			fn cast(node: SyntaxNode) -> Option<Self> {
				match node.kind() {
					$( SyntaxKind::$kind => Some(Self::$kind($kind::cast(node)?)), )+
					_ => Some(Self::$else($else::cast(node)?)),
				}
			}

			fn syntax(&self) -> SyntaxNode {
				match self {
					   Self::$else(s) => s.syntax(),
					$( Self::$kind(s) => s.syntax(), )+
				}
			}
		}
	};
}

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

pub(crate) trait AstNode: Sized {
    fn cast(node: SyntaxNode) -> Option<Self>;

    fn syntax(&self) -> SyntaxNode;

    fn text(&self) -> String {
        self.syntax().text().to_string()
    }

    fn span(&self) -> TextRange {
        self.syntax().text_range()
    }
}

pub(crate) trait AstToken: Sized {
    fn cast(node: SyntaxToken) -> Option<Self>;

    fn syntax(&self) -> SyntaxToken;

    fn text(&self) -> String {
        self.syntax().text().to_string()
    }

    fn span(&self) -> TextRange {
        self.syntax().text_range()
    }
}

fn first_ident(node: &impl AstNode) -> Option<String> {
    node.syntax()
        .children_with_tokens()
        .filter_map(SyntaxElement::into_token)
        .find(|token| matches!(token.kind(), SyntaxKind::Ident | SyntaxKind::OpIdent))
        .map(|token| token.text().into())
}

fn first_child<Parent: AstNode, Child: AstNode>(parent: &Parent) -> Option<Child> {
    parent.syntax().children().find_map(Child::cast)
}

fn children<Parent: AstNode, Child: AstNode>(parent: &Parent) -> Vec<Child> {
    parent.syntax().children().filter_map(Child::cast).collect()
}

fn match_node<Parent: AstNode>(parent: &Parent, st: SyntaxKind) -> Option<SyntaxNode> {
    parent.syntax().children().find(|node| node.kind() == st)
}

fn match_nodes<Parent: AstNode>(
    parent: &Parent,
    st: SyntaxKind,
) -> impl Iterator<Item = SyntaxNode> {
    parent
        .syntax()
        .children()
        .filter(move |node| node.kind() == st)
}
