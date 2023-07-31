pub mod ast;
mod operators;
mod parse;
mod syntax_kind;

pub(crate) use self::syntax_kind::SyntaxKind;
use self::syntax_kind::SyntaxKind::*;
use crate::{
    diagnostic::{AsSourceLocation, SourceId, SourceLocation},
    session::Session,
    syntax::{ast::AstNode, parse::parse_raw},
};

//--------------------------------------------------------------------------------------------------

/// Rowan language definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}
impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= ROOT as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;
pub type SyntaxToken = rowan::SyntaxToken<Lang>;

impl AsSourceLocation for SyntaxToken {
    fn source_location(&self) -> SourceLocation {
        SourceLocation::new(None, self.text_range())
    }
}

impl AsSourceLocation for SyntaxNode {
    fn source_location(&self) -> SourceLocation {
        SourceLocation::new(None, self.text_range())
    }
}

/// Main entry point for parsing.
pub fn parse(session: &mut Session, text: &str, file: SourceId) -> ast::Root {
    let node = parse_raw(session, text, file);
    ast::Root {
        source_id: file,
        module: ast::Module::cast(node).unwrap(),
    }
}

//--------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::{
        diagnostic::SourceFileProvider,
        syntax,
        syntax::{ast::AstNode, Diagnostics, SyntaxNode},
        Session,
    };
    use codespan_reporting::{
        term,
        term::termcolor::{ColorChoice, StandardStream},
    };
    use std::num::FpCategory::Zero;

    fn parse_source_text(text: &str) -> SyntaxNode {
        let mut sess = Session::new();
        let (package, _) = sess.get_or_create_package("test");
        let package = sess.create_source_package("test", "test", text);
        sess.get_ast(package).module.syntax().clone()
    }

    fn expr(expr: &str) -> SyntaxNode {
        let src = format!("void main() {{ {expr}; }}");
        parse_source_text(&src)
    }

    #[test]
    fn empty_test() {
        insta::assert_debug_snapshot!(parse_source_text(r#"    "#));
    }

    #[test]
    fn whitespace_tests() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
void main(){}
void main2    (    )    {     }
void main3(    float arg    ) { }
void main4(  float      arg) {}
"#
        ));
    }

    #[test]
    fn basic_parser_tests() {
        insta::assert_debug_snapshot!(
            "empty function",
            parse_source_text(
                r#"
void main() {
}

int test() {
}

// array types
float[4] test3() {
}
"#
            )
        );

        insta::assert_debug_snapshot!(
            "empty functions with arguments",
            parse_source_text(
                r#"
void zero_arg() {}
void one_arg(int a) {}
void two_args(float a, float b) {}
"#
            )
        );
    }

    #[test]
    fn expr_parser_tests() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
void expr_test() {
    0;
    a;
    0 + 1;
    a + 1;
    1 + 2 * 3;
    1 * 2 + 3 * 4;
    1 + 2 * 3 + 4;
    1 + a[i];
    a[i+1];
    a[i][j];
    -a[j];
    1 + -2 + 3;
    -x(1);
    -x[1];
    1 + 2 * 3 % 4 - 5 / 6;
    1 + 2 * 3;
    1 << 2 + 3;
    1 & 2 >> 3;
    1 ^ 2 & 3;
    1 | 2 ^ 3;
    1 == 2 | 3;
    1 && 2 == 3;
    1 || 2 && 2;
    ---!1 - --2 * 9;
    1 * (2 + 3) * 4;
    1 + 2 * (3 + 4);
    f(1 + 2 * (3 + 4))(4)[0];
    return 0;
}
"#
        ));

        insta::assert_debug_snapshot!(expr("f(1, 10.0, f(10), test, 1 + 2, tests()[4], tests()[f(0)])"));

        insta::assert_debug_snapshot!(expr("f(1, 10.0, f(10), tests()[4], tests()[f(0)])"));
    }

    #[test]
    fn control_flow_statements() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
void main() {
    if (one) {
        ok();
    }

    if (one) {
        ok();
    } else {
        not_ok();
    }

    if (one) {
    } else if two {
    } else if three {
    } else {
    }

    if (one) {
    } else if two {
    } else if three {
    }

    while (true) {
    }
}
"#
        ));
    }

    #[test]
    fn variables() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
// global variables
in vec3 v_position;
out vec3 f_position = vec3(0.0);
uniform mat4 u_model_matrix;
buffer float[] data;

void main() {
    float x = 0;
    float x = 0;
    float y = 0;
    float z;
    float w;

    if (true) {
        float x = 0;
        float x = 0;
    }

    if (false) {
        float x = 0;
        float x = 0;
    } else {
        float x = 0;
        float x = 0;
    }

    while (true) {
        float x = 0;
        float x = 0;
    }
}
"#
        ));
    }

    #[test]
    fn structs() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
struct EmptyStruct {}
struct OneField {
    float a;
}
struct TwoFields {
    float a;
    float b;
}
"#
        ));
    }

    #[test]
    fn imports() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
import package;
import package2 as p;
import package3("test") as p1;
import package3  (   "test"   )  as p1  ;
import package4  ;
"#
        ));
    }

    #[test]
    fn function_decl() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
extern void main ( ) ;
       void main ( ) ;
extern void main ( ) { }
public void main ( ) { }
"#
        ));
    }

    #[test]
    fn type_constructors() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
void main() {
    float[3] v = float[3](1, 2, 3);
    float[3][4] v2 = float[3][4](float[3](0.0,0.0,0.0), float[3](0.0,0.0,0.0), float[3](0.0,0.0,0.0), float[3](0.0,0.0,0.0));
    float x = float(0);
    int y = int(x);
}
    "#
        ));
    }
}
