use anyhow::{Result, anyhow};

pub fn css(input: &str) -> Result<String> {
    use css_minify::optimizations::{Level, Minifier};

    let output = Minifier::default()
        .minify(input, Level::One)
        // MError points back to input, so must share its lifetime,
        // unless we stringify it:
        .map_err(|e| anyhow!("CSS minification failed: {:?}", e))?;
    Ok(output)
}

pub fn html<Data: AsRef<[u8]>>(input: Data) -> Result<Vec<u8>> {
    let cfg = minify_html::Cfg {
        do_not_minify_doctype: true,
        ensure_spec_compliant_unquoted_attribute_values: true,
        keep_closing_tags: true,
        keep_html_and_head_opening_tags: true,
        keep_spaces_between_attributes: true,
        keep_comments: false,
        minify_css: true,
        minify_js: true,
        remove_bangs: false,
        remove_processing_instructions: false,
    };
    let output = minify_html::minify(input.as_ref(), &cfg);
    Ok(output)
}
