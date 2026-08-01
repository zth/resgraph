// This file holds the actual language server implementation.

@module("url") external fileURLToPath: string => string = "fileURLToPath"
type fileUrl = {href: string}

@module("url") external pathToFileURL: string => fileUrl = "pathToFileURL"

let ensureFileUri = path =>
  if path->String.startsWith("file://") {
    path
  } else {
    pathToFileURL(path).href
  }

let initialized = ref(false)
let shutdownRequestAlreadyReceived = ref(false)

module Console = Stdlib.Console

let log = Console.error

module Message = {
  type msg
  type requestId

  type t = msg

  type method<'a> = [
    | #initialize
    | #exit
    | #shutdown
    | #"textDocument/didOpen"
    | #"textDocument/didChange"
    | #"textDocument/didClose"
    | #"textDocument/hover"
    | #"textDocument/codeLens"
    | #"textDocument/documentLink"
    | #"textDocument/completion"
    | #"textDocument/codeAction"
    | #"textDocument/definition"
  ] as 'a

  let jsonrpcVersion = "2.0"

  @module("vscode-jsonrpc/lib/messages.js")
  external isNotificationMessage: t => bool = "isNotificationMessage"

  @module("vscode-jsonrpc/lib/messages.js")
  external isRequestMessage: t => bool = "isRequestMessage"

  @get
  external getMethod: t => method<'a> = "method"

  @get
  external unsafeGetParams: t => 'a = "params"

  @get
  external getId: t => requestId = "id"

  module LspMessage = {
    @live
    type textDocumentItem = {
      uri: string,
      languageId: string,
      version: int,
      text: string,
    }

    type textDocumentIdentifier = {uri: string}
    type textDocumentPosition = {textDocument: textDocumentIdentifier, position: LspProtocol.loc}

    @live
    type textDocumentContentChangeEvent = {
      range: option<LspProtocol.range>,
      rangeLength: option<int>,
      text: string,
    }

    type didOpenTextDocumentParams = {textDocument: textDocumentItem}
    type didChangeTextDocumentParams = {
      textDocument: textDocumentItem,
      contentChanges: array<textDocumentContentChangeEvent>,
    }
    type didCloseTextDocumentParams = {textDocument: textDocumentItem}
    type hoverParams = textDocumentPosition
    type codeLensParams = {textDocument: textDocumentIdentifier}
    type documentLinkParams = {textDocument: textDocumentIdentifier}
    type completionParams = textDocumentPosition
    type codeActionParams = {textDocument: textDocumentIdentifier, range: LspProtocol.range}
    type definitionParams = textDocumentPosition

    type t =
      | DidOpenTextDocumentNotification(didOpenTextDocumentParams)
      | DidChangeTextDocumentNotification(didChangeTextDocumentParams)
      | DidCloseTextDocumentNotification(didCloseTextDocumentParams)
      | Hover(hoverParams)
      | CodeLens(codeLensParams)
      | DocumentLinks(documentLinkParams)
      | Completion(completionParams)
      | CodeAction(codeActionParams)
      | Definition(definitionParams)
      | UnmappedMessage

    let decodeLspMessage = (msg: msg): t => {
      switch msg->getMethod {
      | #"textDocument/didOpen" => DidOpenTextDocumentNotification(msg->unsafeGetParams)
      | #"textDocument/didChange" => DidChangeTextDocumentNotification(msg->unsafeGetParams)
      | #"textDocument/didClose" => DidCloseTextDocumentNotification(msg->unsafeGetParams)
      | #"textDocument/hover" => Hover(msg->unsafeGetParams)
      | #"textDocument/codeLens" => CodeLens(msg->unsafeGetParams)
      | #"textDocument/documentLink" => DocumentLinks(msg->unsafeGetParams)
      | #"textDocument/completion" => Completion(msg->unsafeGetParams)
      | #"textDocument/codeAction" => CodeAction(msg->unsafeGetParams)
      | #"textDocument/definition" => Definition(msg->unsafeGetParams)
      | _ => UnmappedMessage
      }
    }
  }

  module Notification: {
    @live
    type publishDiagnosticsParams = {
      uri: string,
      diagnostics: array<LspProtocol.diagnostic>,
    }
    type t = PublishDiagnostics(publishDiagnosticsParams)
    let asMessage: t => msg
  } = {
    type publishDiagnosticsParams = {
      uri: string,
      diagnostics: array<LspProtocol.diagnostic>,
    }
    type t = PublishDiagnostics(publishDiagnosticsParams)

    @live
    type notificationMessage<'params> = {
      jsonrpc: string,
      method: [#"textDocument/publishDiagnostics"],
      params: 'params,
    }

    external notificationMessageAsMsg: notificationMessage<'params> => msg = "%identity"

    let asMessage = (notification: t): msg =>
      switch notification {
      | PublishDiagnostics(params) =>
        {
          jsonrpc: jsonrpcVersion,
          method: #"textDocument/publishDiagnostics",
          params,
        }->notificationMessageAsMsg
      }
  }

  module Error: {
    type t
    type code = ServerNotInitialized | InvalidRequest | InternalError
    let make: (~code: code, ~message: string) => t
  } = {
    type code = ServerNotInitialized | InvalidRequest | InternalError
    let codeToInt = code =>
      switch code {
      | ServerNotInitialized => -32002
      | InvalidRequest => -32600
      | InternalError => -32603
      }

    @live
    type t = {
      code: int,
      message: string,
    }

    let make = (~code, ~message) => {
      code: code->codeToInt,
      message,
    }
  }

  module InitializeResult: {
    type t
    @live
    type completionProvider = {triggerCharacters: array<string>}
    type textDocumentSync = Full
    let make: (
      ~textDocumentSync: textDocumentSync=?,
      ~hoverProvider: bool=?,
      ~completionProvider: completionProvider=?,
      ~codeLensProvider: bool=?,
      ~documentLinkProvider: bool=?,
      ~codeActionProvider: bool=?,
      ~definitionProvider: bool=?,
      unit,
    ) => t
  } = {
    type completionProvider = {triggerCharacters: array<string>}
    type textDocumentSync = Full

    let textDocumentSyncToInt = v =>
      switch v {
      | Full => 1
      }

    @live
    type capabilities = {
      textDocumentSync: int,
      hoverProvider: bool,
      completionProvider: option<completionProvider>,
      codeLensProvider: bool,
      documentLinkProvider: bool,
      codeActionProvider: bool,
      definitionProvider: bool,
    }

    @live
    type t = {capabilities: capabilities}

    let make = (
      ~textDocumentSync=Full,
      ~hoverProvider=false,
      ~completionProvider=?,
      ~codeLensProvider=false,
      ~documentLinkProvider=false,
      ~codeActionProvider=false,
      ~definitionProvider=false,
      (),
    ) => {
      capabilities: {
        textDocumentSync: textDocumentSync->textDocumentSyncToInt,
        hoverProvider,
        completionProvider,
        codeLensProvider,
        documentLinkProvider,
        codeActionProvider,
        definitionProvider,
      },
    }
  }

  module Result: {
    type t
    external fromInitialize: InitializeResult.t => t = "%identity"
    external fromHover: LspProtocol.hover => t = "%identity"
    external fromCodeLenses: array<LspProtocol.codeLens> => t = "%identity"
    external fromDocumentLinks: array<LspProtocol.documentLink> => t = "%identity"
    external fromCompletionItems: array<LspProtocol.completionItem> => t = "%identity"
    external fromCodeActions: array<LspProtocol.codeAction> => t = "%identity"
    external fromDefinition: LspProtocol.definition => t = "%identity"
    external fromRoutesForFile: array<LspProtocol.Command.routeRendererReference> => t = "%identity"
    external fromRoutesMatchingUrl: array<LspProtocol.Command.routeRendererReference> => t =
      "%identity"
    let null: unit => t
  } = {
    type t
    external fromAny: 'any => t = "%identity"
    external fromInitialize: InitializeResult.t => t = "%identity"
    external fromHover: LspProtocol.hover => t = "%identity"
    external fromCodeLenses: array<LspProtocol.codeLens> => t = "%identity"
    external fromDocumentLinks: array<LspProtocol.documentLink> => t = "%identity"
    external fromCompletionItems: array<LspProtocol.completionItem> => t = "%identity"
    external fromCodeActions: array<LspProtocol.codeAction> => t = "%identity"
    external fromDefinition: LspProtocol.definition => t = "%identity"
    external fromRoutesForFile: array<LspProtocol.Command.routeRendererReference> => t = "%identity"
    external fromRoutesMatchingUrl: array<LspProtocol.Command.routeRendererReference> => t =
      "%identity"
    let null = () => Nullable.null->fromAny
  }

  module Response: {
    type t
    external asMessage: t => msg = "%identity"
    let make: (~id: requestId, ~error: Error.t=?, ~result: Result.t=?, unit) => t
  } = {
    @live
    type t = {
      jsonrpc: string,
      id: requestId,
      error: option<Error.t>,
      result: option<Result.t>,
    }
    external asMessage: t => msg = "%identity"

    let make = (~id, ~error=?, ~result=?, ()) => {
      jsonrpc: jsonrpcVersion,
      id,
      error,
      result,
    }
  }
}

let defaultSendFn: Message.t => unit = _ => ()

let sendFn = ref(defaultSendFn)
let send = msg => {
  sendFn.contents(msg)
}

type mode = NodeRpc | Stdio

type stdout
type stdin

@val
external stdout: stdout = "process.stdout"

@val
external stdin: stdin = "process.stdin"

type onMessageCallback = Message.t => unit

module Rpc = {
  module StreamMessageWriter = {
    type t

    @new @module("vscode-jsonrpc")
    external make: stdout => t = "StreamMessageWriter"

    @send
    external write: (t, Message.t) => unit = "write"
  }

  module StreamMessageReader = {
    type t

    @new @module("vscode-jsonrpc")
    external make: stdin => t = "StreamMessageReader"

    @send
    external listen: (t, onMessageCallback) => unit = "listen"
  }
}

@val
external processSend: Message.t => unit = "process.send"

@val
external processOnMessage: (@as(json`"message"`) _, onMessageCallback) => unit = "process.on"

@val
external exitProcess: int => unit = "process.exit"

let start = (~mode, ~configFilePath) => {
  let config = switch Utils.readConfigFromDir(configFilePath) {
  | Error(msg) =>
    log(msg)
    panic(msg)
  | Ok(config) => config
  }

  let configIssues = []
  config->InitProject.validateConfig(~issues=configIssues, ~configDir=configFilePath)
  if configIssues->Array.length > 0 {
    configIssues->InitProject.printProjectIssues
    panic("Invalid ResGraph configuration.")
  }

  GeneratedArtifacts.sync(config, ~selectedSchemas=config.schemas, ~configDir=configFilePath)

  let currentResults: Dict.t<Utils.callResult> = dict{}

  let resFilesCache: Dict.t<string> = Dict.make()

  let filesWithDiagnostics = ref([])

  let publishDiagnostics = () => {
    let filesWithDiagnosticsAtLastPublish = filesWithDiagnostics.contents->Array.copy
    let errorsByFile: Dict.t<array<Utils.generateError>> = dict{}

    config.schemas->Array.forEach(schema =>
      switch currentResults->Dict.get(schema.name) {
      | Some(Error({errors})) =>
        errors->Array.forEach(error => {
          let errorsForFile = errorsByFile->Dict.get(error.file)->Option.getOr([])
          errorsByFile->Dict.set(error.file, errorsForFile->Array.concat([error]))
        })
      | _ => ()
      }
    )

    let currentFilesWithDiagnostics =
      errorsByFile
      ->Dict.toArray
      ->Array.map(((file, errors)) => {
        PublishDiagnostics({
          uri: file->ensureFileUri,
          diagnostics: errors->Array.map(error => {
            let diagnostic: LspProtocol.diagnostic = {
              range: error.range,
              message: error.message,
              source: "ResGraph",
            }
            diagnostic
          }),
        })
        ->Message.Notification.asMessage
        ->send
        file->ensureFileUri
      })

    filesWithDiagnostics := currentFilesWithDiagnostics

    filesWithDiagnosticsAtLastPublish->Array.forEach(fileName => {
      if !(currentFilesWithDiagnostics->Array.includes(fileName)) {
        PublishDiagnostics({uri: fileName->ensureFileUri, diagnostics: []})
        ->Message.Notification.asMessage
        ->send
      }
    })
  }

  let watchers = config.schemas->Array.map(schema =>
    Utils.setupWatcher(
      ~onStartRebuild=_schema => (),
      ~onResult=(schema, result) =>
        switch result {
        | Utils.GeneratorResult(res) =>
          currentResults->Dict.set(schema.name, res)
          publishDiagnostics()
        | Utils.GeneratorProcessFailure =>
          currentResults->Dict.delete(schema.name)
          publishDiagnostics()
        },
      ~config=schema,
    )
  )

  let openedFile = (uri, text) => {
    log(`opened ${uri}`)
    switch uri->Path.extname {
    | ".res" | ".resi" | ".graphql" => resFilesCache->Dict.set(uri, text)
    | _ => ()
    }
  }

  let updateOpenedFile = (uri, text) => {
    if [".res", ".resi", ".graphql"]->Array.includes(uri->Path.extname) {
      switch resFilesCache->Dict.get(uri)->Option.isSome {
      | true => resFilesCache->Dict.set(uri, text)
      | false => ()
      }
    }
  }

  let closeFile = uri => {
    resFilesCache->Dict.delete(uri)
  }

  let onMessage = msg => {
    if Message.isNotificationMessage(msg) {
      switch (initialized.contents, msg->Message.getMethod) {
      | (true, method) =>
        switch method {
        | #exit =>
          if shutdownRequestAlreadyReceived.contents === true {
            exitProcess(0)
          } else {
            exitProcess(1)
          }

        | _ =>
          switch msg->Message.LspMessage.decodeLspMessage {
          | DidOpenTextDocumentNotification(params) =>
            openedFile(params.textDocument.uri, params.textDocument.text)
          | DidChangeTextDocumentNotification(params) =>
            switch params.contentChanges->Array.copy->Array.pop {
            | Some({text}) => updateOpenedFile(params.textDocument.uri, text)
            | _ => ()
            }
          | DidCloseTextDocumentNotification(params) => closeFile(params.textDocument.uri)
          | _ => ()
          }
        }
        ()
      | _ => log("Could not handle notification message.")
      }
    } else if Message.isRequestMessage(msg) {
      let sendNullresponse = () => {
        Message.Response.make(~id=msg->Message.getId, ~result=Message.Result.null(), ())
        ->Message.Response.asMessage
        ->send
      }
      switch (initialized.contents, msg->Message.getMethod) {
      | (false, method) if method !== #initialize =>
        Message.Response.make(
          ~id=msg->Message.getId,
          ~error=Message.Error.make(~code=ServerNotInitialized, ~message=`Server not initialized.`),
          (),
        )
        ->Message.Response.asMessage
        ->send
      | (false, #initialize) =>
        initialized := true
        Message.Response.make(
          ~id=msg->Message.getId,
          ~result=Message.InitializeResult.make(
            ~completionProvider={triggerCharacters: ["@", "~"]},
            ~hoverProvider=true,
            ~textDocumentSync=Full,
            ~definitionProvider=true,
            (),
          )->Message.Result.fromInitialize,
          (),
        )
        ->Message.Response.asMessage
        ->send

      | (true, method) =>
        switch method {
        | #initialize => sendNullresponse()
        | #shutdown =>
          if shutdownRequestAlreadyReceived.contents === true {
            Message.Response.make(
              ~id=msg->Message.getId,
              ~error=Message.Error.make(
                ~code=InvalidRequest,
                ~message=`Language server already received the shutdown request.`,
              ),
              (),
            )
            ->Message.Response.asMessage
            ->send
          } else {
            shutdownRequestAlreadyReceived := true
            watchers->Array.forEach(watcher =>
              watcher->Bindings.Chokidar.Watcher.close->Promise.ignore
            )
            Message.Response.make(~id=msg->Message.getId, ~result=Message.Result.null(), ())
            ->Message.Response.asMessage
            ->send
          }
        | _ =>
          switch msg->Message.LspMessage.decodeLspMessage {
          | Hover(params) =>
            let filePath = params.textDocument.uri->fileURLToPath
            switch params.textDocument.uri->Path.extname {
            | ".graphql" =>
              let stateName =
                config
                ->Utils.schemaForGraphqlFile(filePath)
                ->Option.flatMap(schema => schema.stateName)
              let result = switch LspCompleteGraphQL.hoverAtPos(
                ~path=filePath,
                ~text=?resFilesCache->Dict.get(params.textDocument.uri),
                ~pos=params.position,
                ~stateName,
              ) {
              | Some(hover) => hover->Message.Result.fromHover
              | None => Message.Result.null()
              }
              Message.Response.make(~id=msg->Message.getId, ~result, ())
              ->Message.Response.asMessage
              ->send
            | ".res" | ".resi" =>
              let result = switch Utils.callPrivateCli(
                Hover({filePath, position: params.position}),
              ) {
              | Hover({item}) => Message.Result.fromHover(item)
              | _ => Message.Result.null()
              }
              Message.Response.make(~id=msg->Message.getId, ~result, ())
              ->Message.Response.asMessage
              ->send
            | _ => sendNullresponse()
            }
          | Definition(params) =>
            let filePath = params.textDocument.uri->fileURLToPath
            switch params.textDocument.uri->Path.extname {
            | ".graphql" =>
              let stateName =
                config
                ->Utils.schemaForGraphqlFile(filePath)
                ->Option.flatMap(schema => schema.stateName)
              let result = switch LspCompleteGraphQL.definitionAtPos(
                ~path=filePath,
                ~text=?resFilesCache->Dict.get(params.textDocument.uri),
                ~pos=params.position,
                ~stateName,
              ) {
              | Some(definition) => definition->Message.Result.fromDefinition
              | None => Message.Result.null()
              }
              Message.Response.make(~id=msg->Message.getId, ~result, ())
              ->Message.Response.asMessage
              ->send
            | _ => sendNullresponse()
            }
          | Completion(params) =>
            switch params.textDocument.uri->Path.extname {
            | ".graphql" =>
              Message.Response.make(~id=msg->Message.getId, ~result=Message.Result.null(), ())
              ->Message.Response.asMessage
              ->send
            | _ =>
              switch resFilesCache->Dict.get(params.textDocument.uri) {
              | None =>
                Message.Response.make(~id=msg->Message.getId, ~result=Message.Result.null(), ())
                ->Message.Response.asMessage
                ->send
              | Some(code) =>
                let filePath = params.textDocument.uri->fileURLToPath
                let stateName =
                  config
                  ->Utils.schemaForFile(filePath)
                  ->Option.flatMap(schema => schema.stateName)
                let result = Utils.withTemporaryFile(~contents=code, tmpname =>
                  switch Utils.callPrivateCli(
                    Completion({filePath, position: params.position, tmpname, ?stateName}),
                  ) {
                  | Completion({items}) => Message.Result.fromCompletionItems(items)
                  | _ => Message.Result.null()
                  }
                )
                Message.Response.make(~id=msg->Message.getId, ~result, ())
                ->Message.Response.asMessage
                ->send
              }
            }
          | _ =>
            Message.Response.make(
              ~id=msg->Message.getId,
              ~error=Message.Error.make(
                ~code=InvalidRequest,
                ~message=`Unrecognized editor request.`,
              ),
              (),
            )
            ->Message.Response.asMessage
            ->send
          }
        }

      | _ =>
        Message.Response.make(
          ~id=msg->Message.getId,
          ~error=Message.Error.make(~code=InvalidRequest, ~message=`Unrecognized editor request.`),
          (),
        )
        ->Message.Response.asMessage
        ->send
      }
    }
  }

  let onMessageSafely = msg => {
    let sendInternalError = () => {
      if Message.isRequestMessage(msg) {
        Message.Response.make(
          ~id=msg->Message.getId,
          ~error=Message.Error.make(
            ~code=InternalError,
            ~message="ResGraph language server request failed.",
          ),
          (),
        )
        ->Message.Response.asMessage
        ->send
      }
    }

    try {
      onMessage(msg)
    } catch {
    | Exn.Error(error) =>
      log(error)
      sendInternalError()
    | _ =>
      log("Unknown ResGraph language server request failure.")
      sendInternalError()
    }
  }

  // ////
  // BOOT
  // ////

  switch mode {
  | Stdio =>
    let writer = Rpc.StreamMessageWriter.make(stdout)
    let reader = Rpc.StreamMessageReader.make(stdin)
    sendFn := (msg => writer->Rpc.StreamMessageWriter.write(msg))
    reader->Rpc.StreamMessageReader.listen(onMessageSafely)
    log(`Starting LSP in stdio mode.`)

  | NodeRpc =>
    sendFn := processSend
    processOnMessage(onMessageSafely)
    log(`Starting LSP in Node RPC.`)
  }
}

external process: 'any = "process"

process
->Process.onUnhandledRejection((err, _p) => {
  Console.error(err)
})
->ignore
