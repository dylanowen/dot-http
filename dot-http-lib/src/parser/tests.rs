use super::*;
use crate::parser;

#[macro_export]
macro_rules! assert_variant {
    ($tested_enum:expr, $expected:pat) => {
        assert_variant!($tested_enum, $expected, ())
    };
    ($tested_enum:expr, $expected:pat, $variant_test:expr) => {
        match $tested_enum {
            $expected => $variant_test,
            unexpected => panic!(
                "Expected {} but found {:?}",
                stringify!($expected),
                unexpected
            ),
        }
    };
}

#[test]
fn weird_file() {
    let test = "\
POST http://example.com HTTP/1.1

{}

> {% console.log('no'); %}";

    let file = parser::parse(test).unwrap();
    assert_eq!(file.request_scripts.len(), 1);

    if let RequestScript {
        request:
            Request {
                method,
                target,
                headers,
                body: Some(body),
                ..
            },
        handler: Some(Handler { script, .. }),
        ..
    } = &file.request_scripts[0]
    {
        assert_variant!(method, Method::Post(_));
        assert_variant!(
            &target.state,
            Unprocessed::WithoutInline(s, _),
            assert_eq!(s, "http://example.com")
        );
        assert_eq!(headers.len(), 0);
        assert_variant!(
            &body.state,
            Unprocessed::WithoutInline(s, _),
            assert_eq!(s, "{}\n\n")
        );

        assert_eq!(script, "console.log('no');");
    } else {
        panic!("Expected certain values for our RequestScript but couldn't find them");
    }
}

#[test]
fn empty_body_with_handler() {
    let test = "\
POST http://example.com HTTP/1.1
Accept: */*

> {%
    console.log('cool');
%}
###
";

    let file = parser::parse(test).unwrap();
    assert_eq!(file.request_scripts.len(), 1);

    if let RequestScript {
        request:
            Request {
                method,
                target,
                headers,
                body: None,
                ..
            },
        handler: Some(Handler { script, .. }),
        ..
    } = &file.request_scripts[0]
    {
        assert_variant!(method, Method::Post(_));
        assert_variant!(
            &target.state,
            Unprocessed::WithoutInline(s, _),
            assert_eq!(s, "http://example.com")
        );
        assert_eq!(headers.len(), 1);
        assert_eq!(headers[0].field_name, "Accept");
        assert_variant!(
            &headers[0].field_value.state,
            Unprocessed::WithoutInline(s, _),
            assert_eq!(s, "*/*")
        );

        assert_eq!(script, "console.log('cool');");
    } else {
        panic!("Expected certain values for our RequestScript but couldn't find them");
    }
}

#[test]
fn new_line_in_request_body_file() {
    let test = "\
POST http://example.com HTTP/1.1
Accept: */*

{
    \"test\": \"a\",
    \"what\": [

    ]
}

> {%
    console.log('cool');
%}

###
";
    let file = parser::parse(test).unwrap();
    assert_eq!(file.request_scripts.len(), 1);

    if let RequestScript {
        request:
            Request {
                method,
                target,
                headers,
                body: Some(body),
                ..
            },
        handler: Some(Handler { script, .. }),
        ..
    } = &file.request_scripts[0]
    {
        assert_variant!(method, Method::Post(_));
        assert_variant!(
            &target.state,
            Unprocessed::WithoutInline(s, _),
            assert_eq!(s, "http://example.com")
        );
        assert_eq!(headers.len(), 1);
        assert_eq!(headers[0].field_name, "Accept");
        assert_variant!(
            &headers[0].field_value.state,
            Unprocessed::WithoutInline(s, _),
            assert_eq!(s, "*/*")
        );
        assert_variant!(
            &body.state,
            Unprocessed::WithoutInline(s, _),
            assert_eq!(
                s,
                "{\n    \"test\": \"a\",\n    \"what\": [\n\n    ]\n}\n\n"
            )
        );

        assert_eq!(script, "console.log('cool');");
    } else {
        panic!("Expected certain values for our RequestScript but couldn't find them");
    }
}

#[test]
fn mixing_body_and_headers() {
    let test = "\
GET http://example.com HTTP/1.1
header: some-value";

    let file = parser::parse(test).unwrap();
    assert_eq!(file.request_scripts.len(), 1);

    if let RequestScript {
        request:
            Request {
                method,
                target,
                headers,
                body: None,
                ..
            },
        handler: None,
        ..
    } = &file.request_scripts[0]
    {
        assert_variant!(method, Method::Get(_));
        assert_variant!(
            &target.state,
            Unprocessed::WithoutInline(s, _),
            assert_eq!(s, "http://example.com")
        );
        assert_eq!(headers.len(), 1);
        assert_eq!(headers[0].field_name, "header");
        assert_variant!(
            &headers[0].field_value.state,
            Unprocessed::WithoutInline(s, _),
            assert_eq!(s, "some-value")
        );
    } else {
        panic!("Expected certain values for our RequestScript but couldn't find them");
    }
}

#[test]
fn multiple_request_scripts() {
    let test = "\
POST http://example.com HTTP/1.1
header: some-value

###

POST http://example.com HTTP/1.1

{}

> {% console.log('no'); %}";

    let file = parser::parse(test).unwrap();
    assert_eq!(file.request_scripts.len(), 2);

    for request_script in &file.request_scripts {
        let RequestScript {
            request: Request { method, target, .. },
            ..
        } = request_script;
        assert_variant!(method, Method::Post(_));
        assert_variant!(
            &target.state,
            Unprocessed::WithoutInline(s, _),
            assert_eq!(s, "http://example.com")
        );
    }

    if let RequestScript {
        request: Request {
            headers,
            body: None,
            ..
        },
        handler: None,
        ..
    } = &file.request_scripts[0]
    {
        assert_eq!(headers.len(), 1);
        assert_eq!(headers[0].field_name, "header");
        assert_variant!(
            &headers[0].field_value.state,
            Unprocessed::WithoutInline(s, _),
            assert_eq!(s, "some-value")
        );
    } else {
        panic!("Expected certain values for our RequestScript but couldn't find them");
    }

    if let RequestScript {
        request:
            Request {
                headers,
                body: Some(body),
                ..
            },
        handler: Some(Handler { script, .. }),
        ..
    } = &file.request_scripts[1]
    {
        assert_eq!(headers.len(), 0);
        assert_variant!(
            &body.state,
            Unprocessed::WithoutInline(s, _),
            assert_eq!(s, "{}\n\n")
        );
        assert_eq!(script, "console.log('no');");
    } else {
        panic!("Expected certain values for our RequestScript but couldn't find them");
    }
}
