use std::fmt::{self, Display};

use abi_stable::{
    export_root_module,
    external_types::crossbeam_channel::RSender,
    prefix_type::PrefixTypeTrait,
    sabi_extern_fn,
    sabi_trait::prelude::TD_Opaque,
    std_types::{ROk, RResult, RStr, RVec},
};
use dwarf::{
    chacha::{error::ChaChaError, ffi_value::FfiValue},
    plug_in::{Error, LambdaCall, Plugin, PluginModRef, PluginModule, PluginType, Plugin_TO},
};
use html_escape::decode_html_entities;
use markdown::{CompileOptions, ParseOptions};
use regex::Regex;

#[export_root_module]
pub fn instantiate_root_module() -> PluginModRef {
    PluginModule { name, new }.leak_into_prefix()
}

#[sabi_extern_fn]
pub fn name() -> RStr<'static> {
    "md".into()
}

/// Instantiates the plugin.
#[sabi_extern_fn]
pub fn new(
    lambda_sender: RSender<LambdaCall>,
    _args: RVec<FfiValue>,
) -> RResult<PluginType, Error> {
    let plugin = md::instantiate_root_module();
    let plugin = plugin.new();
    let plugin = plugin(lambda_sender, vec![].into()).unwrap();
    ROk(Plugin_TO::from_value(plugin, TD_Opaque))
}

mod md {
    use super::*;

    pub fn instantiate_root_module() -> PluginModRef {
        PluginModule { name, new }.leak_into_prefix()
    }

    #[sabi_extern_fn]
    pub fn name() -> RStr<'static> {
        "md".into()
    }

    /// Instantiates the plugin.
    #[sabi_extern_fn]
    pub fn new(
        _lambda_sender: RSender<LambdaCall>,
        _args: RVec<FfiValue>,
    ) -> RResult<PluginType, Error> {
        ROk(Plugin_TO::from_value(Md::default(), TD_Opaque))
    }

    #[derive(Clone, Debug)]
    struct Md {
        links: Regex,
        code: Regex,
        h1: Regex,
        h2: Regex,
        h3: Regex,
        h4: Regex,
    }

    impl Default for Md {
        fn default() -> Self {
            Self {
                links: Regex::new("<a href=\"#(.*?)\">").unwrap(),
                code: Regex::new(
                    r#"<pre><code class="language-mermaid">\s*([\s\S]*?)\s*</code></pre>"#,
                )
                .unwrap(),
                h1: Regex::new(r#"<h1>(.*?)</h1>"#).unwrap(),
                h2: Regex::new(r#"<h2>(.*?)</h2>"#).unwrap(),
                h3: Regex::new(r#"<h3>(.*?)</h3>"#).unwrap(),
                h4: Regex::new(r#"<h4>(.*?)</h4>"#).unwrap(),
            }
        }
    }

    impl Display for Md {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    impl Plugin for Md {
        fn name(&self) -> RStr<'_> {
            "Md".into()
        }

        #[tracing::instrument]
        fn invoke_func(
            &self,
            module: RStr<'_>,
            ty: RStr<'_>,
            func: RStr<'_>,
            args: RVec<FfiValue>,
        ) -> RResult<FfiValue, Error> {
            match ty.as_str() {
                "Md" => match func.as_str() {
                    "to_html" => {
                        let md: String = args
                        .first()
                        .unwrap()
                        .try_into()
                        .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                        .unwrap();

                    let options = markdown::Options {
                        parse: ParseOptions::gfm(),
                        compile: CompileOptions {
                            allow_dangerous_html: true,
                            ..CompileOptions::gfm()
                        },
                    };

                    let md = markdown::to_html_with_options(&md, &options).unwrap();

                    // Post-process the HTML to handle internal links
                    let md = self.links.replace_all(&md, |caps: &regex::Captures| {
                        let id = &caps[1];
                        let id = id.to_lowercase().replace(" ", "-");
                        format!(r#"<a href="javascript:void(0)" onclick="history.pushState(null, null, window.location.href); window.scrollTo({{top: document.getElementById('{id}').offsetTop, behavior: 'smooth'}});">"#)
                    });

                    // Post-process the HTML to add id attributes to section headers
                    let md = self.h1.replace_all(&md, |caps: &regex::Captures| {
                        let title = &caps[1];
                        let id = title.to_lowercase().replace(" ", "-");
                        format!(r#"<h1 id="{}">{}</h1>"#, id, title)
                    });

                    let md = self.h2.replace_all(&md, |caps: &regex::Captures| {
                        let title = &caps[1];
                        let id = title.to_lowercase().replace(" ", "-");
                        format!(r#"<h2 id="{}">{}</h2>"#, id, title)
                    });

                    let md = self.h3.replace_all(&md, |caps: &regex::Captures| {
                        let title = &caps[1];
                        let id = title.to_lowercase().replace(" ", "-");
                        format!(r#"<h3 id="{}">{}</h3>"#, id, title)
                    });

                    let md = self.h4.replace_all(&md, |caps: &regex::Captures| {
                        let title = &caps[1];
                        let id = title.to_lowercase().replace(" ", "-");
                        format!(r#"<h4 id="{}">{}</h4>"#, id, title)
                    });

                    // Here's where we handle mermaid diagrams
                    let md = if md.contains("class=\"language-mermaid\"") {
                        // Replace the <pre><code> with what mermaid wants
                    let md = self.code.replace_all(&md, |caps: &regex::Captures| {
                        let code = &caps[1];
                        let code = decode_html_entities(code).to_string();

                        // Unescape the code block
                        format!("<pre class='mermaid'>\n{code}\n</pre>")
                    });

                    let mermaid = r#"<script type="module">
                        import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';
                        mermaid.initialize({ startOnLoad: true, theme: 'dark' });
                    </script>"#;

                    format!("{}\n{}", md, mermaid)
                } else {
                    md.to_string()
                };

                Ok(FfiValue::String(md.into()).into())
                }
                func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
            },
            ty => Err(Error::Plugin(format!("Invalid type: {ty}").into())),
        }
        .into()
        }

        #[tracing::instrument]
        fn invoke_func_mut(
            &mut self,
            module: RStr<'_>,
            ty: RStr<'_>,
            func: RStr<'_>,
            args: RVec<FfiValue>,
        ) -> RResult<FfiValue, Error> {
            Ok(FfiValue::Empty).into()
        }
    }
}
