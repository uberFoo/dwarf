use ansi_term::Colour;
use dap::prelude::*;
use snafu::prelude::*;

// Error handling
const C_ERR: Colour = Colour::Red;
const _C_OK: Colour = Colour::Green;
const _C_WARN: Colour = Colour::Yellow;
const _C_OTHER: Colour = Colour::Cyan;

pub type Result<T, E = DapAdapterError> = std::result::Result<T, E>;

#[derive(Debug, Snafu)]
pub enum DapAdapterError {
    /// Unhandled Command
    ///
    /// A DAP command that we don't handle.
    #[snafu(display("\n{}: unhandled command `{}`", C_ERR.bold().paint("error"), command))]
    UnhandledCommand { command: String },
}

pub struct DapAdapter;

impl Adapter for DapAdapter {
    type Error = DapAdapterError;

    fn accept(
        &mut self,
        request: Request,
        _ctx: &mut dyn Context,
    ) -> Result<Response, Self::Error> {
        match &request.command {
            Command::Initialize(args) => {
                if let Some(client_name) = args.client_name.as_ref() {
                    println!("> Client '{client_name}' requested initialization.");
                    Ok(Response::make_success(
                        &request,
                        ResponseBody::Initialize(Some(types::Capabilities {
                            supports_configuration_done_request: Some(true),
                            supports_evaluate_for_hovers: Some(true),
                            ..Default::default()
                        })),
                    ))
                } else {
                    Ok(Response::make_error(&request, "Missing client name"))
                }
            }
            Command::Next(_) => Ok(Response::make_ack(&request).unwrap()),
            cmd => Err(DapAdapterError::UnhandledCommand {
                command: format!("{:?}", cmd),
            }),
        }
    }
}
