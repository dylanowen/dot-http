use std::fmt::Display;
use std::{error, fmt};

use serde::export::Formatter;

use pest::error::Error as PestError;
use pest::error::LineColLocation;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::Parser;
use pest::Span;

#[cfg(test)]
pub mod pest_tests;
#[cfg(test)]
pub mod tests;

#[derive(Parser)]
#[grammar = "parser/parser.pest"]
struct ScriptParser;

pub fn parse(source: &str) -> Result<File, Error> {
    let mut pairs = ScriptParser::parse(Rule::file, source)?;

    let file = pairs
        .expect_pair(Rule::file, Selection::none)
        .and_then(File::from_pair)?;

    pairs.expect_end()?;

    Ok(file)
}

trait FromPair: Sized {
    fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, Error>;
}

impl FromPair for File {
    fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, Error> {
        Rule::file.expect(&pair)?;

        let mut pairs = pair.into_inner();
        let request_scripts = pairs
            .take_pairs(Rule::request_script)
            .into_iter()
            .map(RequestScript::from_pair)
            .collect::<Result<Vec<RequestScript>, Error>>()?;

        pairs.expect_end()?;

        Ok(File { request_scripts })
    }
}

impl FromPair for RequestScript {
    fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, Error> {
        Rule::request_script.expect(&pair)?;

        let selection: Selection = pair.as_span().into();
        let mut pairs = pair.into_inner();

        let request = parse_request(&mut pairs, selection.clone())?;
        let handler = match pairs.take_pair(Rule::response_handler) {
            Some(pair) => Some(Handler::from_pair(pair)?),
            None => None,
        };

        pairs.expect_end()?;

        Ok(RequestScript {
            selection,
            request,
            handler,
        })
    }
}

fn parse_request(pairs: &mut Pairs<'_, Rule>, selection: Selection) -> Result<Request, Error> {
    let method = pairs
        .expect_pair(Rule::method, || selection.eoi())
        .and_then(Method::from_pair)?;

    let target = pairs
        .expect_pair(Rule::request_target, || selection.eoi())
        .and_then(Value::from_pair)?;

    let headers = pairs
        .take_pairs(Rule::header_field)
        .into_iter()
        .map(Header::from_pair)
        .collect::<Result<Vec<Header>, Error>>()?;

    let body = match pairs.take_pair(Rule::request_body) {
        Some(pair) => Some(Value::from_pair(pair)?),
        None => None,
    };

    Ok(Request {
        selection,
        method,
        target,
        headers,
        body,
    })
}

impl FromPair for Method {
    fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, Error> {
        Rule::method.expect(&pair)?;

        let selection: Selection = pair.as_span().into();
        match pair.as_str() {
            "GET" => Ok(Method::Get(selection)),
            "POST" => Ok(Method::Post(selection)),
            "DELETE" => Ok(Method::Delete(selection)),
            "PUT" => Ok(Method::Put(selection)),
            "PATCH" => Ok(Method::Patch(selection)),
            "OPTIONS" => Ok(Method::Options(selection)),
            _ => Err(Error::new(
                format!("Unsupported method: {}", pair.as_str()),
                pair.as_span(),
            )),
        }
    }
}

impl FromPair for Header {
    fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, Error> {
        Rule::header_field.expect(&pair)?;

        let selection: Selection = pair.as_span().into();
        let mut pairs = pair.into_inner();

        let field_name = pairs
            .expect_pair(Rule::field_name, || selection.eoi())?
            .as_str()
            .to_string();
        let field_value = pairs
            .expect_pair(Rule::field_value, || selection.eoi())
            .and_then(Value::from_pair)?;

        pairs.expect_end()?;

        Ok(Header {
            selection,
            field_name,
            field_value,
        })
    }
}

impl FromPair for Handler {
    fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, Error> {
        Rule::response_handler.expect(&pair)?;

        let selection: Selection = pair.as_span().into();
        let mut pairs = pair.into_inner();

        let handler_script_pair = pairs.expect_pair(Rule::handler_script, || selection.eoi())?;
        let script = parse_handler(handler_script_pair, selection.clone())?;

        pairs.expect_end()?;

        Ok(Handler { selection, script })
    }
}

fn parse_handler(
    handler_script_pair: Pair<'_, Rule>,
    selection: Selection,
) -> Result<String, Error> {
    let mut pairs = handler_script_pair.into_inner();
    let script_string = pairs
        .expect_pair(Rule::handler_script_string, || selection.eoi())?
        .as_str()
        .to_string();

    pairs.expect_end()?;

    Ok(script_string)
}

impl FromPair for Value {
    fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, Error> {
        match (pair.as_rule(), pair.as_str()) {
            (Rule::request_target, string)
            | (Rule::field_value, string)
            | (Rule::request_body, string) => {
                let selection = pair.as_span().into();
                let mut pairs = pair.into_inner();

                let inline_scripts = pairs
                    .take_pairs(Rule::inline_script)
                    .into_iter()
                    .map(InlineScript::from_pair)
                    .collect::<Result<Vec<InlineScript>, Error>>()?;

                pairs.expect_end()?;

                if !inline_scripts.is_empty() {
                    Ok(Value {
                        state: Unprocessed::WithInline {
                            value: string.to_string(),
                            inline_scripts,
                            selection,
                        },
                    })
                } else {
                    Ok(Value {
                        state: Unprocessed::WithoutInline(string.to_string(), selection),
                    })
                }
            }
            _ => Rule::request_target.invalid_pair(&pair),
        }
    }
}

impl FromPair for InlineScript {
    fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, Error> {
        Rule::inline_script.expect(&pair)?;

        let selection: Selection = pair.as_span().into();
        let placeholder = pair.as_str().to_string();

        let mut pairs = pair.into_inner();

        let script = pairs
            .expect_pair(Rule::inline_script_string, || selection.eoi())
            .map(|pair| pair.as_str().to_string())?;

        pairs.expect_end()?;

        Ok(InlineScript {
            selection,
            placeholder,
            script,
        })
    }
}

#[derive(Debug)]
pub enum Error {
    PestError(PestError<Rule>),
    CustomError {
        message: String,
        selection: Selection,
    },
}

impl Error {
    pub fn new<M, Sel>(message: M, selection: Sel) -> Self
    where
        M: ToString,
        Sel: Into<Selection>,
    {
        Self::CustomError {
            message: message.to_string(),
            selection: selection.into(),
        }
    }

    pub fn message(&self) -> String {
        match self {
            Error::PestError(pest_error) => pest_error.variant.message().to_string(),
            Error::CustomError { message, .. } => message.clone(),
        }
    }

    pub fn selection(&self) -> Selection {
        match self {
            Error::PestError(pest_error) => pest_error.line_col.clone().into(),
            Error::CustomError { selection, .. } => selection.clone(),
        }
    }
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::PestError(pest_error) => write!(fmt, "{}", pest_error),
            Error::CustomError { message, .. } => write!(fmt, "{}", message),
        }
    }
}

impl From<PestError<Rule>> for Error {
    fn from(pest_error: PestError<Rule>) -> Self {
        Error::PestError(pest_error)
    }
}

impl Into<Selection> for LineColLocation {
    fn into(self) -> Selection {
        match self {
            LineColLocation::Pos((line, col)) => Selection {
                start: Position { line, col },
                end: Position { line, col },
            },
            LineColLocation::Span((start_line, start_col), (end_line, end_col)) => Selection {
                start: Position {
                    line: start_line,
                    col: start_col,
                },
                end: Position {
                    line: end_line,
                    col: end_col,
                },
            },
        }
    }
}

impl Into<Selection> for Span<'_> {
    fn into(self) -> Selection {
        let (start_line, start_col) = self.start_pos().line_col();
        let (end_line, end_col) = self.end_pos().line_col();
        Selection {
            // filename,
            start: Position {
                line: start_line,
                col: start_col,
            },
            end: Position {
                line: end_line,
                col: end_col,
            },
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.state {
            Unprocessed::WithInline { value, .. } => f.write_str(&value),
            Unprocessed::WithoutInline(value, _) => f.write_str(&value),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Value {
    pub state: Unprocessed,
}

#[derive(Debug, Clone)]
pub enum Unprocessed {
    WithInline {
        value: String,
        inline_scripts: Vec<InlineScript>,
        selection: Selection,
    },
    WithoutInline(String, Selection),
}

#[derive(Debug, Clone)]
pub struct InlineScript {
    pub script: String,
    pub placeholder: String,
    pub selection: Selection,
}

#[derive(Debug)]
pub struct File {
    pub request_scripts: Vec<RequestScript>,
}

#[derive(Debug, Clone)]
pub struct RequestScript {
    pub request: Request,
    pub handler: Option<Handler>,
    pub selection: Selection,
}

#[derive(Debug, Clone)]
pub struct Request {
    pub method: Method,
    pub target: Value,
    pub headers: Vec<Header>,
    pub body: Option<Value>,
    pub selection: Selection,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Method {
    Get(Selection),
    Post(Selection),
    Delete(Selection),
    Put(Selection),
    Patch(Selection),
    Options(Selection),
}

#[derive(Debug, Clone)]
pub struct Header {
    pub field_name: String,
    pub field_value: Value,
    pub selection: Selection,
}

#[derive(Debug, Clone)]
pub struct Handler {
    pub script: String,
    pub selection: Selection,
}

impl File {
    pub fn request_scripts(
        &self,
        offset: usize,
        all: bool,
    ) -> crate::Result<impl Iterator<Item = &RequestScript>> {
        let mut scripts = self
            .request_scripts
            .iter()
            .filter(move |request_script| {
                (all || request_script.selection.start.line <= offset)
                    && request_script.selection.end.line > offset
            })
            .peekable();

        match scripts.peek() {
            Some(_) => Ok(scripts),
            None => Err(anyhow!(
                "Couldn't find any scripts in our file at the given line number"
            )),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Selection {
    pub start: Position,
    pub end: Position,
}

impl Selection {
    pub fn none() -> Selection {
        Selection {
            start: Position { line: 0, col: 0 },
            end: Position { line: 0, col: 0 },
        }
    }

    /// Return the end of input selection but using the end position as our selection
    fn eoi(&self) -> Selection {
        Selection {
            start: self.end.clone(),
            end: self.end.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

impl Rule {
    fn expect(self, pair: &Pair<'_, Rule>) -> Result<(), Error> {
        if pair.as_rule() == self {
            Ok(())
        } else {
            self.invalid_pair(pair)
        }
    }

    fn invalid_pair<T>(self, got: &Pair<'_, Rule>) -> Result<T, Error> {
        Err(Error::new(
            format!(
                "Invalid Token. Expected: {:?}, Got: {:?}",
                self,
                got.as_rule()
            ),
            got.as_span(),
        ))
    }
}

trait EnhancedPairs<'a> {
    fn expect_pair<F>(&mut self, rule: Rule, eoi_selection: F) -> Result<Pair<'a, Rule>, Error>
    where
        F: FnOnce() -> Selection;

    fn take_pair(&mut self, rule: Rule) -> Option<Pair<'a, Rule>>;

    fn take_pairs(&mut self, rule: Rule) -> Vec<Pair<'a, Rule>> {
        let mut result = Vec::new();
        while let Some(pair) = self.take_pair(rule) {
            result.push(pair);
        }

        result
    }

    fn expect_end(&mut self) -> Result<(), Error>;
}

impl<'a> EnhancedPairs<'a> for Pairs<'a, Rule> {
    fn expect_pair<F>(&mut self, rule: Rule, eoi_selection: F) -> Result<Pair<'a, Rule>, Error>
    where
        F: FnOnce() -> Selection,
    {
        if let Some(pair) = self.next() {
            rule.expect(&pair).map(|_| pair)
        } else {
            Err(Error::new(
                format!("Expected: {:?}, but found the end of input", rule),
                eoi_selection(),
            ))
        }
    }

    fn take_pair(&mut self, rule: Rule) -> Option<Pair<'a, Rule>> {
        if self.peek().map(|p| p.as_rule()) == Some(rule) {
            self.next()
        } else {
            None
        }
    }

    fn expect_end(&mut self) -> Result<(), Error> {
        match self.next() {
            None => Ok(()),
            Some(unexpected_pair) => {
                if unexpected_pair.as_rule() == Rule::EOI {
                    Ok(())
                } else {
                    Err(Error::new(
                        format!(
                            "No more tokens expected, but found: {:?}",
                            unexpected_pair.as_rule()
                        ),
                        unexpected_pair.as_span(),
                    ))
                }
            }
        }
    }
}
