use nom::AsChar;
use rustyline::completion::{Completer, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::hint::{Hinter, HistoryHinter};
use rustyline::validate::{
    MatchingBracketValidator, ValidationContext, ValidationResult, Validator,
};
use rustyline::{Cmd, CompletionType, Config, Context, Editor, KeyEvent};
use rustyline_derive::Helper;
use std::borrow::Cow::{self, Borrowed, Owned};

static RESERVE_WORDS: [&str; 9] = [
    "begin", "set", "val", "define", "if", "lambda", "let", "let*", "letrec",
];

pub struct REPL {
    pub rl: Editor<InputValidator>,
}

impl REPL {
    pub fn new() -> REPL {
        let config = Config::builder()
            .history_ignore_space(true)
            .completion_type(CompletionType::List)
            .build();
        let mut rl = rustyline::Editor::with_config(config);
        let helper = Some(InputValidator {
            brackets: MatchingBracketValidator::new(),
            highlighter: MatchingBracketHighlighter::new(),
            hinter: HistoryHinter {},
            colored_prompt: "".to_owned(),
        });
        rl.set_helper(helper);
        rl.bind_sequence(KeyEvent::alt('f'), Cmd::HistorySearchForward);
        rl.bind_sequence(KeyEvent::alt('b'), Cmd::HistorySearchBackward);
        if rl.load_history("history.txt").is_err() {
            println!("No previous history.");
        }
        REPL { rl }
    }
}

#[derive(Helper)]
pub struct InputValidator {
    pub brackets: MatchingBracketValidator,
    pub highlighter: MatchingBracketHighlighter,
    pub hinter: HistoryHinter,
    pub colored_prompt: String,
}

impl Hinter for InputValidator {
    type Hint = String;

    fn hint(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Option<String> {
        self.hinter.hint(line, pos, ctx)
    }
}

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        self.brackets.validate(ctx)
    }
    fn validate_while_typing(&self) -> bool {
        self.brackets.validate_while_typing()
    }
}

impl Highlighter for InputValidator {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Borrowed(&self.colored_prompt)
        } else {
            Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}

impl Completer for InputValidator {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Pair>), ReadlineError> {
        let mut new_pos = (pos - 1) as i64;
        let chars = line.chars();
        let res = line;
        if line.len() == 0 {
            return Ok((
                pos,
                RESERVE_WORDS
                    .to_vec()
                    .into_iter()
                    .map(|w| Pair {
                        display: String::from(w),
                        replacement: format!("{}", w),
                    })
                    .collect(),
            ));
        }
        loop {
            if new_pos == -1 {
                new_pos = 0;
                break;
            } else {
                let v = chars.clone().nth(new_pos as usize);
                match v {
                    Some(v) => {
                        if v.is_alpha() {
                            new_pos -= 1;
                        } else {
                            new_pos += 1;
                            break;
                        }
                    }
                    None => break,
                }
            }
        }

        let candidates: Vec<String> = RESERVE_WORDS
            .iter()
            .filter(|w| w.contains(&res[new_pos as usize..]))
            .map(|w| String::from(w.clone()))
            .collect();

        Ok((
            new_pos as usize,
            candidates
                .iter()
                .map(|w| Pair {
                    display: String::from(w),
                    replacement: format!(
                        "{}",
                        w.as_str()
                            .strip_suffix(&res[(new_pos - 1) as usize..])
                            .unwrap_or(w)
                            .to_string()
                    ),
                })
                .collect(),
        ))
    }
}
