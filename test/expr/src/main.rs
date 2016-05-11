mod expr;

use expr::ParserContext;


fn main() {
    let src = "14+(34*123)".to_owned();

    let mut parser = ParserContext::new(&src);

    loop {
        match parser.next_lexical_token() {
            Ok(lu_opt) => match lu_opt {
                None => { break; }
                Some(token) => { println!("Matched : {} \"{}\" ({}:{})", token.name, token.matched_text, token.start, token.end); }
            },
            Err(msg) => {
                println!("{}", msg);
                break;
            }
        }
    }
}
