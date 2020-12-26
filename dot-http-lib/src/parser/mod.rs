use std::fmt;
use std::fmt::Display;
use std::path::PathBuf;

use serde::export::Formatter;

pub use pest::error;
use pest::iterators::Pair;
use pest::Parser;
use pest::Span;

use crate::parser::error::{Error, ErrorVariant};

#[cfg(test)]
pub mod tests;

#[derive(Parser)]
#[grammar = "parser/parser.pest"]
struct ScriptParser;

trait FromPair: Sized {
    fn from_pair(filename: PathBuf, pair: Pair<'_, Rule>) -> Result<Self, Error<Rule>>;

    fn invalid_pair(expected: Rule, got: &Pair<'_, Rule>) -> Result<Self, Error<Rule>> {
        Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: format!(
                    "Wrong pair. Expected: {:?}, Got: {:?}",
                    expected,
                    got.as_rule()
                ),
            },
            got.as_span(),
        ))
    }
}

trait ToSelection {
    fn to_selection(self, filename: PathBuf) -> Selection;
}

impl FromPair for Handler {
    fn from_pair(filename: PathBuf, pair: Pair<'_, Rule>) -> Result<Self, Error<Rule>> {
        match pair.as_rule() {
            Rule::response_handler => {
                let selection = pair.as_span().to_selection(filename);
                let script = pair
                    .into_inner()
                    .find_map(|pair| match pair.as_rule() {
                        Rule::handler_script => Some(
                            pair.into_inner()
                                .find_map(|pair| match pair.as_rule() {
                                    Rule::handler_script_string => Some(pair.as_str()),
                                    _ => None,
                                })
                                .unwrap(),
                        ),
                        _ => None,
                    })
                    .unwrap()
                    .to_string();

                Ok(Handler { selection, script })
            }
            _ => Self::invalid_pair(Rule::response_handler, &pair),
        }
    }
}

impl FromPair for Method {
    fn from_pair(filename: PathBuf, pair: Pair<'_, Rule>) -> Result<Self, Error<Rule>> {
        let selection = pair.as_span().to_selection(filename);
        match pair.as_rule() {
            Rule::method => match pair.as_str() {
                "GET" => Ok(Method::Get(selection)),
                "POST" => Ok(Method::Post(selection)),
                "DELETE" => Ok(Method::Delete(selection)),
                "PUT" => Ok(Method::Put(selection)),
                "PATCH" => Ok(Method::Patch(selection)),
                "OPTIONS" => Ok(Method::Options(selection)),
                _ => Err(pest::error::Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: format!("Unsupported method: {}", pair.as_str()),
                    },
                    pair.as_span(),
                )),
            },
            _ => Self::invalid_pair(Rule::method, &pair),
        }
    }
}

impl FromPair for Value {
    fn from_pair(filename: PathBuf, pair: Pair<'_, Rule>) -> Result<Self, Error<Rule>> {
        match (pair.as_rule(), pair.as_str()) {
            (Rule::request_target, string)
            | (Rule::field_value, string)
            | (Rule::request_body, string) => {
                let selection = pair.as_span().clone().to_selection(filename.clone());
                let inline_scripts = pair
                    .into_inner()
                    .filter(|pair| pair.as_rule() == Rule::inline_script)
                    .map(|pair| InlineScript::from_pair(filename.clone(), pair))
                    .collect::<Result<Vec<InlineScript>, Error<Rule>>>()?;

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
            _ => Self::invalid_pair(Rule::request_target, &pair),
        }
    }
}

impl FromPair for InlineScript {
    fn from_pair(filename: PathBuf, pair: Pair<'_, Rule>) -> Result<Self, Error<Rule>> {
        match pair.as_rule() {
            Rule::inline_script => {
                let selection = pair.as_span().to_selection(filename);
                let placeholder = pair.as_str().to_string();
                let script = pair
                    .into_inner()
                    .map(|pair| pair.as_str())
                    .last()
                    .unwrap()
                    .to_string();

                Ok(InlineScript {
                    selection,
                    placeholder,
                    script,
                })
            }
            _ => Self::invalid_pair(Rule::inline_script, &pair),
        }
    }
}

impl FromPair for Header {
    fn from_pair(filename: PathBuf, pair: Pair<'_, Rule>) -> Result<Self, Error<Rule>> {
        match pair.as_rule() {
            Rule::header_field => {
                let selection = pair.as_span().clone().to_selection(filename.clone());
                let mut pairs = pair.into_inner();
                let field_name = pairs
                    .find_map(|pair| match pair.as_rule() {
                        Rule::field_name => Some(pair.as_str().to_string()),
                        _ => None,
                    })
                    .unwrap();
                let field_value = pairs
                    .find_map(|pair| match pair.as_rule() {
                        Rule::field_value => Some(Value::from_pair(filename.clone(), pair)),
                        _ => None,
                    })
                    .unwrap()?;

                Ok(Header {
                    selection,
                    field_name,
                    field_value,
                })
            }
            _ => Self::invalid_pair(Rule::header_field, &pair),
        }
    }
}

impl FromPair for Request {
    fn from_pair(filename: PathBuf, pair: Pair<'_, Rule>) -> Result<Self, Error<Rule>> {
        match pair.as_rule() {
            Rule::request_script => {
                let selection = pair.as_span().clone().to_selection(filename.clone());
                let mut pairs = pair.into_inner();
                let method = pairs
                    .find_map(|pair| match pair.as_rule() {
                        Rule::method => Some(Method::from_pair(filename.clone(), pair)),
                        _ => None,
                    })
                    .unwrap()?;
                let target = pairs
                    .find_map(|pair| match pair.as_rule() {
                        Rule::request_target => Some(Value::from_pair(filename.clone(), pair)),
                        _ => None,
                    })
                    .unwrap()?;
                let headers = pairs
                    .clone()
                    .filter_map(|pair| match pair.as_rule() {
                        Rule::header_field => Some(Header::from_pair(filename.clone(), pair)),
                        _ => None,
                    })
                    .collect::<Result<Vec<Header>, Error<Rule>>>()?;
                let body = {
                    let pair = pairs.find_map(|pair| match pair.as_rule() {
                        Rule::request_body => Some(pair),
                        _ => None,
                    });

                    if let Some(pair) = pair {
                        Some(Value::from_pair(filename, pair)?)
                    } else {
                        None
                    }
                };

                Ok(Request {
                    selection,
                    method,
                    target,
                    headers,
                    body,
                })
            }
            _ => Self::invalid_pair(Rule::request_script, &pair),
        }
    }
}

impl FromPair for RequestScript {
    fn from_pair(filename: PathBuf, pair: Pair<'_, Rule>) -> Result<Self, Error<Rule>> {
        match pair.as_rule() {
            Rule::request_script => {
                let mut pairs = pair.clone().into_inner();
                Ok(RequestScript {
                    selection: pair.as_span().to_selection(filename.clone()),
                    request: Request::from_pair(filename.clone(), pair)?,
                    handler: {
                        let pair = pairs.find_map(|pair| match pair.as_rule() {
                            Rule::response_handler => Some(pair),
                            _ => None,
                        });
                        if let Some(pair) = pair {
                            Some(Handler::from_pair(filename, pair)?)
                        } else {
                            None
                        }
                    },
                })
            }
            _ => Self::invalid_pair(Rule::request_script, &pair),
        }
    }
}

impl FromPair for File {
    fn from_pair(filename: PathBuf, pair: Pair<'_, Rule>) -> Result<Self, Error<Rule>> {
        match pair.as_rule() {
            Rule::file => {
                let request_scripts = pair
                    .into_inner()
                    .filter(|pair| pair.as_rule() == Rule::request_script)
                    .map(|pair| RequestScript::from_pair(filename.clone(), pair))
                    .collect::<Result<Vec<RequestScript>, Error<Rule>>>()?;
                Ok(File { request_scripts })
            }
            _ => Self::invalid_pair(Rule::file, &pair),
        }
    }
}

impl ToSelection for Span<'_> {
    fn to_selection(self, filename: PathBuf) -> Selection {
        let (start_line, start_col) = self.start_pos().line_col();
        let (end_line, end_col) = self.end_pos().line_col();
        Selection {
            filename,
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

pub fn parse(filename: PathBuf, source: &str) -> Result<File, Error<Rule>> {
    ScriptParser::parse(Rule::file, source)?
        .map(|pair| File::from_pair(filename.clone(), pair))
        .last()
        .unwrap()
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

impl Selection {
    pub fn none() -> Selection {
        Selection {
            filename: PathBuf::default(),
            start: Position { line: 0, col: 0 },
            end: Position { line: 0, col: 0 },
        }
    }
}

impl File {
    pub fn request_scripts(
        &self,
        offset: usize,
        all: bool,
    ) -> impl Iterator<Item = &RequestScript> {
        let mut scripts = self
            .request_scripts
            .iter()
            .filter(move |request_script| {
                (all || request_script.selection.start.line <= offset)
                    && request_script.selection.end.line > offset
            })
            .peekable();

        match scripts.peek() {
            Some(_) => scripts,
            None => panic!("Couldn't find any scripts in our file at the given line number"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Selection {
    pub filename: PathBuf,
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}
