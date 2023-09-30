use alloy_syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
use std::fmt;
use text_size::TextRange;

macro_rules! ast_node {
    ($kind:ident, fields: [$($field:ident),+]) => {
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
                    $( .field(stringify!($field), &self.$field()) )+
                    .finish()
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

pub use module_definition::*;
mod module_definition;

pub use type_definition::*;
mod type_definition;

pub use behavior::*;
mod behavior;

pub use type_variable::*;
mod type_variable;

pub use path::*;
mod path;

pub trait AstNode: Sized {
    fn cast(node: SyntaxNode) -> Option<Self>;

    fn syntax(&self) -> SyntaxNode;

    fn text(&self) -> String {
        self.syntax().text().to_string()
    }

    fn span(&self) -> TextRange {
        self.syntax().text_range()
    }
}
