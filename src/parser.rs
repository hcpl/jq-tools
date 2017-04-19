use nom::IResult;

use ast::*;
use nom_helpers::*;
use lexer::*;


named!(pub top_level<&str, TopLevel>, ws!(alt_complete!(
    do_parse!(
        module: module >>
        imports: many0!(import) >>
        expr: expr >>
        (TopLevel::WithExpr { module: module, imports: imports, expr: expr })
    ) |
    do_parse!(
        module: module >>
        imports: many0!(import) >>
        func_defs: many0!(func_def) >>
        (TopLevel::NoExpr { module: module, imports: imports, func_defs: func_defs })
    )
)));

named!(pub module<&str, Module>, ws!(alt_complete!(
    do_parse!(
        tag!("module") >>
        metadata: expr >>
        char!(';') >>
        (Module::WithMetadata { metadata: metadata })
    ) |
    do_parse!(
        (Module::None)
    )
)));

named!(pub import<&str, Import>, ws!(alt_complete!(
    do_parse!(
        what: import_what >>
        metadata: expr >>
        (Import::WithMetadata { what: what, metadata: metadata })
    ) |
    do_parse!(
        what: import_what >>
        (Import::Simple { what: what })
    )
)));

named!(pub import_what<&str, ImportWhat>, ws!(alt_complete!(
    do_parse!(
        from: import_from >>
        as_ident: identifier >>
        (ImportWhat::Json { from: from, as_ident: as_ident.to_string() })
    ) |
    do_parse!(
        from: import_from >>
        as_ident: identifier >>
        (ImportWhat::Jq { from: from, as_ident: as_ident.to_string() })
    ) |
    do_parse!(
        from: import_from >>
        (ImportWhat::Include { from: from })
    )
)));

named!(pub import_from<&str, ImportFrom>, ws!(alt_complete!(
    do_parse!(
        path: lstring >>
        (ImportFrom::ImportFrom { path: path })
    )
)));

named!(pub expr<&str, Box<Expr>>, ws!(alt_complete!(
    do_parse!(
        func_def: func_def >>
        expr: expr >>
        (Box::new(Expr::Top { func_def: func_def, expr: expr }))
    ) |
    do_parse!(
        term_patterns: term_patterns >>
        char!('|') >>
        expr: expr >>
        (Box::new(Expr::TermPipe { term_patterns: term_patterns, expr: expr }))
    ) |
    do_parse!(
        tag!("reduce") >>
        term_patterns: term_patterns >>
        char!('(') >>
        base: expr >>
        char!(';') >>
        increment: expr >>
        char!(')') >>
        (Box::new(Expr::Reduce { term_patterns: term_patterns, base: base, increment: increment }))
    ) |
    do_parse!(
        tag!("foreach") >>
        term_patterns: term_patterns >>
        char!('(') >>
        init: expr >>
        char!(';') >>
        update: expr >>
        char!(';') >>
        extract: expr >>
        char!(')') >>
        (Box::new(Expr::ForeachEx { term_patterns: term_patterns, init: init, update: update, extract: extract }))
    ) |
    do_parse!(
        tag!("foreach") >>
        term_patterns: term_patterns >>
        char!('(') >>
        init: expr >>
        char!(';') >>
        update: expr >>
        char!(')') >>
        (Box::new(Expr::Foreach { term_patterns: term_patterns, init: init, update: update }))
    ) |
    do_parse!(
        tag!("if") >>
        init_cond: expr >>
        tag!("then") >>
        init_case: expr >>
        elif_pairs: many0!(do_parse!(
            tag!("elif") >>
            elif_cond: expr >>
            tag!("then") >>
            elif_case: expr >>
            (elif_cond, elif_case)
        )) >>
        tag!("else") >>
        else_case: expr >>
        tag!("end") >>
        (Box::new(Expr::Conditional {
            if_then_cases: {
                let mut elif_pairs = elif_pairs;
                elif_pairs.insert(0, (init_cond, init_case));
                elif_pairs
            },
            else_case: else_case,
        }))
    ) |
    do_parse!(
        tag!("try") >>
        try: expr >>
        tag!("catch") >>
        catch: expr >>
        (Box::new(Expr::TryCatch { try: try, catch: catch }))
    ) |
    do_parse!(
        tag!("try") >>
        try: expr >>
        (Box::new(Expr::Try { try: try }))
    ) |
    do_parse!(
        tag!("label") >>
        char!('$') >>
        ident: identifier >>
        char!('|') >>
        expr: expr >>
        (Box::new(Expr::LabelPipe { ident: ident.to_string(), expr: expr }))
    ) |
    do_parse!(
        operator_expr: operator_expr >>
        (Box::new(Expr::WithOperator { operator_expr: operator_expr }))
    )
)));

named!(pub term_patterns<&str, TermPatterns>, ws!(alt_complete!(
    do_parse!(
        term: term >>
        tag!("as") >>
        patterns: patterns >>
        (TermPatterns { term: term, patterns: patterns })
    )
)));

named!(pub operator_expr<&str, OperatorExpr>, ws!(alt_complete!(
    do_parse!(
        op: unary_prefix_op >>
        operand: expr >>
        (OperatorExpr::UnaryPrefix { op: op, operand: operand })
    ) |
    do_parse!(
        operand: expr >>
        op: unary_postfix_op >>
        (OperatorExpr::UnaryPostfix { op: op, operand: operand })
    ) |
    do_parse!(
        left: expr >>
        op: binary_op >>
        right: expr >>
        (OperatorExpr::Binary { op: op, left: left, right: right })
    )
)));

named!(pub unary_prefix_op<&str, UnaryPrefixOp>, do_parse!(char!('?') >> (UnaryPrefixOp::Optional)));
named!(pub unary_postfix_op<&str, UnaryPostfixOp>, do_parse!(char!('-') >> (UnaryPostfixOp::Negate)));
named!(pub binary_op<&str, BinaryOp>, alt_complete!(
    do_parse!(tag!("=")   >> (BinaryOp::Assign))            |
    do_parse!(tag!("or")  >> (BinaryOp::Or))                |
    do_parse!(tag!("and") >> (BinaryOp::And))               |
    do_parse!(tag!("//")  >> (BinaryOp::Alternative))       |
    do_parse!(tag!("//=") >> (BinaryOp::UpdateAlternative)) |
    //do_parse!(tag!("|=")  >> (BinaryOp::Update))    |
    //do_parse!(tag!("|")   >> (BinaryOp::And))    |
    //do_parse!(tag!(",")   >> (BinaryOp::And))    |
    do_parse!(tag!("+")   >> (BinaryOp::Add))               |
    do_parse!(tag!("+=")  >> (BinaryOp::UpdateAdd))         |
    do_parse!(tag!("-")   >> (BinaryOp::Sub))               |
    do_parse!(tag!("-=")  >> (BinaryOp::UpdateSub))         |
    do_parse!(tag!("*")   >> (BinaryOp::Mul))               |
    do_parse!(tag!("*=")  >> (BinaryOp::UpdateMul))         |
    do_parse!(tag!("/")   >> (BinaryOp::Div))               |
    do_parse!(tag!("/=")  >> (BinaryOp::UpdateDiv))         |
    do_parse!(tag!("%")   >> (BinaryOp::Mod))               |
    do_parse!(tag!("%=")  >> (BinaryOp::UpdateMod))         |
    do_parse!(tag!("==")  >> (BinaryOp::Eq))                |
    do_parse!(tag!("!=")  >> (BinaryOp::Neq))               |
    do_parse!(tag!("<")   >> (BinaryOp::Lt))                |
    do_parse!(tag!(">")   >> (BinaryOp::Gt))                |
    do_parse!(tag!("<=")  >> (BinaryOp::Le))                |
    do_parse!(tag!(">=")  >> (BinaryOp::Ge))
));

named!(pub func_def<&str, FuncDef>, ws!(alt_complete!(
    do_parse!(
        tag!("def") >>
        name: identifier >>
        char!(':') >>
        body: expr >>
        char!(';') >>
        (FuncDef::FuncDef { name: name.to_string(), args: Vec::new(), body: body })
    ) |
    do_parse!(
        tag!("def") >>
        name: identifier >>
        char!('(') >>
        args: separated_nonempty_list!(
            char!(';'),
            do_parse!(
                opt!(char!('$')) >>
                ident: identifier >>
                (ident.to_string())
            )
        ) >>
        char!(')') >>
        char!(':') >>
        body: expr >>
        char!(';') >>
        (FuncDef::FuncDef { name: name.to_string(), args: args, body: body })
    )
)));

named!(pub lstring<&str, LString>, alt_complete!(
    do_parse!(
        char!('"') >>
        qq_string: qq_string >>
        char!('"') >>
        (LString::Plain { qq_string: qq_string })
    ) |
    do_parse!(
        format: format >>
        char!('"') >>
        qq_string: qq_string >>
        char!('"') >>
        (LString::Format { format: format, qq_string: qq_string })
    )
));

named!(pub qq_string<&str, QQString>,
    do_parse!(
        parts: many0!(qq_string_part) >>
        (QQString::QQString { parts: parts })
    )
);

// rustc cannot infer E from the return value of that inner macro
fn offending_part(input: &str) -> IResult<&str, &str> {
    do_parse!(input,
        text: take_while1!(apply!(is_in, "^\\\"")) >>
        (text)
    )
}

pub fn qq_string_part(input: &str) -> IResult<&str, QQStringPart> {
    alt_complete!(input,
        do_parse!(
            text: alt_complete!(
                do_parse!(
                    v: many1!(alt_complete!(
                        do_parse!(
                            char!('\\') >>
                            none_of!("u(") >>
                            (1 + 1)
                        ) |
                        do_parse!(
                            tag!("\\u") >>
                            s: take_while_n!(4, char::is_alphanumeric) >>
                            (2 + s.len())
                        )
                    )) >>
                    ({
                        let outlen = v.into_iter().sum();
                        &input[0..outlen]
                    })
                ) |
                offending_part
            ) >>
            (QQStringPart::Text { text: text.to_string() })
        ) |
        do_parse!(
            tag!("\\(") >>
            expr: expr >>
            tag!(")") >>
            (QQStringPart::Interpolation { expr: expr })
        )
    )
}

named!(pub term<&str, Box<Term>>, ws!(alt_complete!(
    do_parse!(
        char!('.') >>
        (Box::new(Term::Self_))
    ) |
    do_parse!(
        tag!("..") >>
        (Box::new(Term::Super))
    ) |
    do_parse!(
        tag!("break") >>
        char!('$') >>
        ident: identifier >>
        (Box::new(Term::Break { ident: ident.to_string() }))
    ) |
    do_parse!(
        term: opt!(term) >>
        char!('.') >>
        field: field >>
        (Box::new(Term::Field { term: term, field: field.to_string() }))
    ) |
    do_parse!(
        term: opt!(term) >>
        char!('.') >>
        str_field: lstring >>
        (Box::new(Term::StringField { term: term, str_field: str_field }))
    ) |
    do_parse!(
        term: term >>
        char!('[') >>
        index: expr >>
        char!(']') >>
        (Box::new(Term::Index { term: term, index: index }))
    ) |
    do_parse!(
        term: term >>
        char!('[') >>
        char!(']') >>
        (Box::new(Term::Expand { term: term }))
    ) |
    do_parse!(
        term: term >>
        char!('[') >>
        from: expr >>
        char!(':') >>
        to: expr >>
        char!(']') >>
        (Box::new(Term::Slice { term: term, from: from, to: to }))
    ) |
    do_parse!(
        term: term >>
        char!('[') >>
        from: expr >>
        char!(':') >>
        char!(']') >>
        (Box::new(Term::SliceFrom { term: term, from: from }))
    ) |
    do_parse!(
        term: term >>
        char!('[') >>
        char!(':') >>
        to: expr >>
        char!(']') >>
        (Box::new(Term::SliceTo { term: term, to: to }))
    ) |
    do_parse!(
        num: literal >>
        (Box::new(Term::NumLiteral { num: num }))
    ) |
    do_parse!(
        lstring: lstring >>
        (Box::new(Term::LString { lstring: lstring }))
    ) |
    do_parse!(
        format: format >>
        (Box::new(Term::Format { format: format }))
    ) |
    do_parse!(
        char!('(') >>
        expr: expr >>
        char!(')') >>
        (Box::new(Term::Parens { expr: expr }))
    ) |
    do_parse!(
        char!('[') >>
        expr: expr >>
        char!(']') >>
        (Box::new(Term::Collect { expr: expr }))
    ) |
    do_parse!(
        char!('[') >>
        char!(']') >>
        (Box::new(Term::EmptyArray))
    ) |
    do_parse!(
        char!('{') >>
        dict_pairs: separated_nonempty_list!(char!(','), dict_pair) >>
        char!('}') >>
        (Box::new(Term::Dict { dict_pairs: dict_pairs }))
    ) |
    do_parse!(
        char!('$') >>
        tag!("__loc__") >>
        (Box::new(Term::Location))
    ) |
    do_parse!(
        char!('$') >>
        name: identifier >>
        (Box::new(Term::Binding { name: name.to_string() }))
    ) |
    do_parse!(
        name: identifier >>
        (Box::new(Term::Var { name: name.to_string() }))
    ) |
    do_parse!(
        name: identifier >>
        char!('(') >>
        args: separated_nonempty_list!(char!(';'), expr) >>
        char!(')') >>
        (Box::new(Term::FuncCall { name: name.to_string(), args: args }))
    )
)));

named!(pub rep_patterns<&str, Box<RepPatterns>>, ws!(alt_complete!(
    do_parse!(
        rep_patterns: rep_patterns >>
        tag!("?//") >>
        pattern: pattern >>
        (Box::new(RepPatterns::Rep { rep_patterns: rep_patterns, pattern: pattern }))
    ) |
    do_parse!(
        pattern: pattern >>
        (Box::new(RepPatterns::Pat { pattern: pattern }))
    )
)));

named!(pub patterns<&str, Patterns>, ws!(alt_complete!(
    do_parse!(
        rep_patterns: rep_patterns >>
        tag!("?//") >>
        pattern: pattern >>
        (Patterns::Rep { rep_patterns: rep_patterns, pattern: pattern })
    ) |
    do_parse!(
        pattern: pattern >>
        (Patterns::Pat { pattern: pattern })
    )
)));

named!(pub pattern<&str, Box<Pattern>>, ws!(alt_complete!(
    do_parse!(
        char!('$') >>
        ident: identifier >>
        (Box::new(Pattern::Single { ident: ident.to_string() }))
    ) |
    do_parse!(
        char!('[') >>
        patterns: separated_nonempty_list!(char!(','), pattern) >>
        char!(']') >>
        (Box::new(Pattern::Array { patterns: patterns }))
    ) |
    do_parse!(
        char!('{') >>
        patterns: separated_nonempty_list!(char!(','), object_pattern) >>
        char!('}') >>
        (Box::new(Pattern::Dict { patterns: patterns }))
    )
)));

named!(pub object_pattern<&str, Box<ObjectPattern>>, ws!(alt_complete!(
    do_parse!(
        char!('$') >>
        ident: identifier >>
        (Box::new(ObjectPattern::AutoBinding { ident: ident.to_string() }))
    ) |
    do_parse!(
        char!('$') >>
        ident: identifier >>
        char!(':') >>
        pattern: pattern >>
        (Box::new(ObjectPattern::WtfBinding { ident: ident.to_string(), pattern: pattern }))
    ) |
    do_parse!(
        ident: identifier >>
        char!(':') >>
        pattern: pattern >>
        (Box::new(ObjectPattern::Binding { ident: ident.to_string(), pattern: pattern }))
    ) |
    do_parse!(
        keyword: keyword >>
        char!(':') >>
        pattern: pattern >>
        (Box::new(ObjectPattern::KeywordBinding { keyword: keyword, pattern: pattern }))
    ) |
    do_parse!(
        lstring: lstring >>
        char!(':') >>
        pattern: pattern >>
        (Box::new(ObjectPattern::StringBinding { lstring: lstring, pattern: pattern }))
    ) |
    do_parse!(
        char!('(') >>
        expr: expr >>
        char!(')') >>
        char!(':') >>
        pattern: pattern >>
        (Box::new(ObjectPattern::ExprBinding { expr: expr, pattern: pattern }))
    )
)));

named!(pub keyword<&str, Keyword>, alt_complete!(
    do_parse!(tag!("as")      >> (Keyword::As))      |
    do_parse!(tag!("def")     >> (Keyword::Def))     |
    do_parse!(tag!("module")  >> (Keyword::Module))  |
    do_parse!(tag!("import")  >> (Keyword::Import))  |
    do_parse!(tag!("include") >> (Keyword::Include)) |
    do_parse!(tag!("if")      >> (Keyword::If))      |
    do_parse!(tag!("then")    >> (Keyword::Then))    |
    do_parse!(tag!("else")    >> (Keyword::Else))    |
    do_parse!(tag!("elif")    >> (Keyword::Elif))    |
    do_parse!(tag!("reduce")  >> (Keyword::Reduce))  |
    do_parse!(tag!("foreach") >> (Keyword::Foreach)) |
    do_parse!(tag!("end")     >> (Keyword::End))     |
    do_parse!(tag!("and")     >> (Keyword::And))     |
    do_parse!(tag!("or")      >> (Keyword::Or))      |
    do_parse!(tag!("try")     >> (Keyword::Try))     |
    do_parse!(tag!("catch")   >> (Keyword::Catch))   |
    do_parse!(tag!("label")   >> (Keyword::Label))   |
    do_parse!(tag!("break")   >> (Keyword::Break))   |
    do_parse!(tag!("__loc__") >> (Keyword::Loc))
));

named!(pub dict_pair<&str, DictPair>, ws!(alt_complete!(
    do_parse!(
        ident: identifier >>
        char!(':') >>
        dict_expr: dict_expr >>
        (DictPair::IdentPair { ident: ident.to_string(), dict_expr: dict_expr })
    ) |
    do_parse!(
        keyword: keyword >>
        char!(':') >>
        dict_expr: dict_expr >>
        (DictPair::KeywordPair { keyword: keyword, dict_expr: dict_expr })
    ) |
    do_parse!(
        lstring: lstring >>
        char!(':') >>
        dict_expr: dict_expr >>
        (DictPair::StringPair { lstring: lstring, dict_expr: dict_expr })
    ) |
    do_parse!(
        lstring: lstring >>
        (DictPair::String { lstring: lstring })
    ) |
    do_parse!(
        char!('$') >>
        ident: identifier >>
        (DictPair::BindingIdent { ident: ident.to_string() })
    ) |
    do_parse!(
        ident: identifier >>
        (DictPair::Ident { ident: ident.to_string() })
    ) |
    do_parse!(
        char!('(') >>
        expr: expr >>
        char!(')') >>
        char!(':') >>
        dict_expr: dict_expr >>
        (DictPair::ExprPair { expr: expr, dict_expr: dict_expr })
    )
)));

named!(pub dict_expr<&str, Box<DictExpr>>, ws!(alt_complete!(
    do_parse!(
        left: dict_expr >>
        char!('|') >>
        right: dict_expr >>
        (Box::new(DictExpr::Join { left: left, right: right }))
    ) |
    do_parse!(
        char!('-') >>
        dict_expr: dict_expr >>
        (Box::new(DictExpr::Negate { dict_expr: dict_expr }))
    ) |
    do_parse!(
        term: term >>
        (Box::new(DictExpr::Term { term: term }))
    )
)));
