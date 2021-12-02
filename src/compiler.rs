use crate::scanner::{Scanner, TokenType};

pub(crate) fn compile(code: &str) {
    let mut scanner = Scanner::new(code);

    loop {
        let token = scanner.scan_token();
        println!("{:?}", token);
        if token.kind == TokenType::Eof {
            break;
        }
    }
}
