use improved_slice_patterns::match_vec;

use alloy_ast as ast;
use alloy_lexer::{Token, TokenKind};

use super::{ParseError, Span, Spanned};

pub fn parse<'a>(
    type_span: &Span,
    input: impl Iterator<Item = Token<'a>>,
) -> Result<(Spanned<ast::Type>, Vec<Token<'a>>), ParseError<'a>> {
    let input = input.collect::<Vec<_>>();
    log::debug!("*parse_type* input: {:?}", &input);

    // let _doc_comments = extract_doc_comments(stream);

    match_vec!(input;
        [
            Token { kind: TokenKind::UpperIdentifier(id), span: id_span },
            Token { kind: TokenKind::Arrow,          span: arrow_span },
            remainder @ ..,
        ] => {
            let (t, remainder) = parse(&(id_span.start..arrow_span.end), remainder)?;

            Ok((Spanned {
                span: id_span.start..t.span.end,
                value: ast::Type::lambda(
                    ast::Type::Identifier(id.to_string()),
                    t.value,
                ),
            }, remainder))
        },

        [
            Token { kind: TokenKind::UpperIdentifier(id), span: id_span },
            remainder @ ..,
        ] => Ok((Spanned {
            span: id_span,
            value: ast::Type::Identifier(id.to_string()),
        }, remainder.collect())),

        [remainder @ ..,] => Err(ParseError::ExpectedType {
            span: type_span.clone(),
            actual: remainder.collect(),
        }),
    )
    .map_err(|remaining| ParseError::ExpectedEOF {
        input: vec![],
        remaining,
    })
    .and_then(|s| s)
}
