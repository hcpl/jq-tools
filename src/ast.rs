use semver::Version;


pub trait Feature {
    fn get_str_version(&self) -> String;

    fn get_version(&self) -> Version {
        Version::parse(&self.get_str_version()).unwrap()
    }

    fn has_str_version(&self, str_version: &str) -> bool {
        self.has_version(&Version::parse(str_version).unwrap())
    }

    fn has_version(&self, version: &Version) -> bool {
        *version >= self.get_version()
    }
}


pub type Identifier = String;
pub type FuncName = Identifier;
pub type VarName = Identifier;
pub type DefArgs = Vec<Identifier>;
pub type CallArgs = Vec<Box<Expr>>;
pub type Format = String;
pub type Field = String;


#[derive(Debug)]
pub enum TopLevel {
    WithExpr { module: Module, imports: Vec<Import>, expr: Box<Expr> },
    NoExpr { module: Module, imports: Vec<Import>, func_defs: Vec<FuncDef> },
}

#[derive(Debug)]
pub enum Module {
    None,
    WithMetadata { metadata: Box<Expr> },
}

#[derive(Debug)]
pub enum Import {
    Simple { what: ImportWhat },
    WithMetadata { what: ImportWhat, metadata: Box<Expr> },
}

#[derive(Debug)]
pub enum ImportWhat {
    Json { from: ImportFrom, as_ident: Identifier },
    Jq { from: ImportFrom, as_ident: Identifier },
    Include { from: ImportFrom },
}

#[derive(Debug)]
pub enum ImportFrom {
    ImportFrom { path: LString },
}

#[derive(Debug)]
pub enum Expr {
    Top { func_def: FuncDef, expr: Box<Expr> },
    TermPipe { term_patterns: TermPatterns, expr: Box<Expr> },
    Reduce { term_patterns: TermPatterns, base: Box<Expr>, increment: Box<Expr> },
    ForeachEx { term_patterns: TermPatterns, init: Box<Expr>, update: Box<Expr>, extract: Box<Expr> },
    Foreach { term_patterns: TermPatterns, init: Box<Expr>, update: Box<Expr> },
    Conditional { if_then_cases: Vec<(Box<Expr>, Box<Expr>)>, else_case: Box<Expr> },
    TryCatch { try: Box<Expr>, catch: Box<Expr> },
    Try { try: Box<Expr> },
    LabelPipe { ident: Identifier, expr: Box<Expr> },
    WithOperator { operator_expr: OperatorExpr },
}

#[derive(Debug)]
pub struct TermPatterns { pub term: Box<Term>, pub patterns: Patterns }

#[derive(Debug)]
pub enum OperatorExpr {
    UnaryPrefix { op: UnaryPrefixOp, operand: Box<Expr> },
    UnaryPostfix { op: UnaryPostfixOp, operand: Box<Expr> },
    Binary { op: BinaryOp, left: Box<Expr>, right: Box<Expr> },
}

#[derive(Debug)]
pub enum UnaryPrefixOp {
    Optional
}

#[derive(Debug)]
pub enum UnaryPostfixOp {
    Negate
}

#[derive(Debug)]
pub enum BinaryOp {
    Assign, Or, And, Alternative, UpdateAlternative, Update,
    Add, UpdateAdd, Sub, UpdateSub, Mul, UpdateMul,
    Div, Mod, UpdateDiv, UpdateMod,
    Eq, Neq, Lt, Gt, Le, Ge,
}

#[derive(Debug)]
pub enum FuncDef {
    FuncDef { name: FuncName, args: DefArgs, body: Box<Expr> },
}

#[derive(Debug)]
pub enum LString {
    Plain { qq_string: QQString },
    Format { format: Format, qq_string: QQString },
}

#[derive(Debug)]
pub enum QQString {
    QQString { parts: Vec<QQStringPart> }
}

#[derive(Debug)]
pub enum QQStringPart {
    Text { text: String },
    Interpolation { expr: Box<Expr> },
}

#[derive(Debug)]
pub enum Term {
    Self_,
    Super,
    Break { ident: Identifier },
    Field { term: Option<Box<Term>>, field: Field },
    StringField { term: Option<Box<Term>>, str_field: LString },
    Index { term: Box<Term>, index: Box<Expr> },
    Expand { term: Box<Term> },
    Slice { term: Box<Term>, from: Box<Expr>, to: Box<Expr> },
    SliceFrom { term: Box<Term>, from: Box<Expr> },
    SliceTo { term: Box<Term>, to: Box<Expr> },
    NumLiteral { num: String },
    LString { lstring: LString },
    Format { format: Format },
    Parens { expr: Box<Expr> },
    Collect { expr: Box<Expr> },
    EmptyArray,
    Dict { dict_pairs: Vec<DictPair> },
    Location,
    Binding { name: Identifier },
    Var { name: VarName },
    FuncCall { name: FuncName, args: CallArgs },
}

#[derive(Debug)]
pub enum RepPatterns {
    Rep { rep_patterns: Box<RepPatterns>, pattern: Box<Pattern> },
    Pat { pattern: Box<Pattern> },
}

#[derive(Debug)]
pub enum Patterns {
    Rep { rep_patterns: Box<RepPatterns>, pattern: Box<Pattern> },
    Pat { pattern: Box<Pattern> },
}

#[derive(Debug)]
pub enum Pattern {
    Single { ident: Identifier },
    Array { patterns: Vec<Box<Pattern>> },
    Dict { patterns: Vec<Box<ObjectPattern>> },
}

#[derive(Debug)]
pub enum ObjectPattern {
    AutoBinding { ident: Identifier },
    WtfBinding { ident: Identifier, pattern: Box<Pattern> },
    Binding { ident: Identifier, pattern: Box<Pattern> },
    KeywordBinding { keyword: Keyword, pattern: Box<Pattern> },
    StringBinding { lstring: LString, pattern: Box<Pattern> },
    ExprBinding { expr: Box<Expr>, pattern: Box<Pattern> },
}

#[derive(Debug)]
pub enum Keyword {
    As, Def, Module, Import, Include, If, Then, Else, Elif,
    Reduce, Foreach, End, And, Or, Try, Catch, Label, Break, Loc,
}

#[derive(Debug)]
pub enum DictPair {
    IdentPair { ident: Identifier, dict_expr: Box<DictExpr> },
    KeywordPair { keyword: Keyword, dict_expr: Box<DictExpr> },
    StringPair { lstring: LString, dict_expr: Box<DictExpr> },
    String { lstring: LString },
    BindingIdent { ident: Identifier },
    Ident { ident: Identifier },
    ExprPair { expr: Box<Expr>, dict_expr: Box<DictExpr> },
}

#[derive(Debug)]
pub enum DictExpr {
    Join { left: Box<DictExpr>, right: Box<DictExpr> },
    Negate { dict_expr: Box<DictExpr> },
    Term { term: Box<Term> },
}
