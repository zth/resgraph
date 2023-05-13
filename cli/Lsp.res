// This file holds the actual language server implementation.

@module("url") external fileURLToPath: string => string = "fileURLToPath"

let initialized = ref(false)
let shutdownRequestAlreadyReceived = ref(false)

let log = Console.error

module Message = {
  type msg

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
  external getId: t => string = "id"

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

    type t =
      | DidOpenTextDocumentNotification(didOpenTextDocumentParams)
      | DidChangeTextDocumentNotification(didChangeTextDocumentParams)
      | DidCloseTextDocumentNotification(didCloseTextDocumentParams)
      | Hover(hoverParams)
      | CodeLens(codeLensParams)
      | DocumentLinks(documentLinkParams)
      | Completion(completionParams)
      | CodeAction(codeActionParams)
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
    type code = ServerNotInitialized | InvalidRequest
    let make: (~code: code, ~message: string) => t
  } = {
    type code = ServerNotInitialized | InvalidRequest
    let codeToInt = code =>
      switch code {
      | ServerNotInitialized => -32002
      | InvalidRequest => -32600
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
      (),
    ) => {
      capabilities: {
        textDocumentSync: textDocumentSync->textDocumentSyncToInt,
        hoverProvider,
        completionProvider,
        codeLensProvider,
        documentLinkProvider,
        codeActionProvider,
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
    external fromRoutesForFile: array<LspProtocol.Command.routeRendererReference> => t = "%identity"
    external fromRoutesMatchingUrl: array<LspProtocol.Command.routeRendererReference> => t =
      "%identity"
    let null = () => Nullable.null->fromAny
  }

  module Response: {
    type t
    external asMessage: t => msg = "%identity"
    let make: (~id: string, ~error: Error.t=?, ~result: Result.t=?, unit) => t
  } = {
    @live
    type t = {
      jsonrpc: string,
      id: string,
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

  let currentResult = ref(Utils.NotInitialized)

  let resFilesCache: Dict.t<string> = Dict.make()

  let filesWithDiagnostics = ref([])

  let publishDiagnostics = () => {
    let filesWithDiagnosticsAtLastPublish = filesWithDiagnostics.contents->Array.copy
    let currentFilesWithDiagnostics = []
    let currentErrors = switch currentResult.contents {
    | Error({errors}) => errors
    | _ => []
    }

    currentErrors->Array.forEach(error => {
      currentFilesWithDiagnostics->Array.push(error.file)

      PublishDiagnostics({
        uri: error.file,
        diagnostics: [
          {
            range: error.range,
            message: error.message,
            source: "ResGraph",
          },
        ],
      })
      ->Message.Notification.asMessage
      ->send
    })

    filesWithDiagnostics := currentFilesWithDiagnostics

    // Delete diagnostics from files that no longer have them
    filesWithDiagnosticsAtLastPublish->Array.forEach(fileName => {
      if !(currentFilesWithDiagnostics->Array.includes(fileName)) {
        PublishDiagnostics({
          uri: fileName,
          diagnostics: [],
        })
        ->Message.Notification.asMessage
        ->send
      }
    })
  }

  let watcher = Utils.setupWatcher(
    ~onStartRebuild=() => (),
    ~onResult=res => {
      currentResult := res
      publishDiagnostics()
    },
    ~config,
  )

  let openedFile = (uri, text) => {
    log(`opened ${uri}`)
    switch uri->Path.extname {
    | ".res" => resFilesCache->Dict.set(uri, text)
    | _ => ()
    }
  }

  let updateOpenedFile = (uri, text) => {
    if uri->Path.extname == ".res" {
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
      switch (initialized.contents, msg->Message.getMethod) {
      | (false, method) if method != #initialize =>
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
            (),
          )->Message.Result.fromInitialize,
          (),
        )
        ->Message.Response.asMessage
        ->send

      | (true, method) =>
        switch method {
        | #initialize =>
          Message.Response.make(~id=msg->Message.getId, ~result=Message.Result.null(), ())
          ->Message.Response.asMessage
          ->send
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
            watcher->Bindings.Chokidar.Watcher.close->Promise.done
            Message.Response.make(~id=msg->Message.getId, ~result=Message.Result.null(), ())
            ->Message.Response.asMessage
            ->send
          }
        | _ =>
          switch msg->Message.LspMessage.decodeLspMessage {
          | Hover(params) =>
            let filePath = params.textDocument.uri->fileURLToPath
            let result = switch Utils.callPrivateCli(Hover({filePath, position: params.position})) {
            | Hover({item}) => Message.Result.fromHover(item)
            | _ => Message.Result.null()
            }
            Message.Response.make(~id=msg->Message.getId, ~result, ())
            ->Message.Response.asMessage
            ->send
          | CodeLens(_params) =>
            Message.Response.make(~id=msg->Message.getId, ~result=Message.Result.null(), ())
            ->Message.Response.asMessage
            ->send
          | DocumentLinks(_params) =>
            Message.Response.make(~id=msg->Message.getId, ~result=Message.Result.null(), ())
            ->Message.Response.asMessage
            ->send
          | Completion(params) =>
            switch resFilesCache->Dict.get(params.textDocument.uri) {
            | None =>
              Message.Response.make(~id=msg->Message.getId, ~result=Message.Result.null(), ())
              ->Message.Response.asMessage
              ->send
            | Some(code) =>
              let filePath = params.textDocument.uri->fileURLToPath
              let tmpname = Utils.createFileInTempDir()
              Fs.writeFileSyncWith(
                tmpname,
                Buffer.fromString(code),
                Fs.writeFileOptions(~encoding="utf-8", ()),
              )
              let result = switch Utils.callPrivateCli(
                Completion({filePath, position: params.position, tmpname}),
              ) {
              | Completion({items}) => Message.Result.fromCompletionItems(items)
              | _ => Message.Result.null()
              }
              Message.Response.make(~id=msg->Message.getId, ~result, ())
              ->Message.Response.asMessage
              ->send
            }

          | CodeAction(_params) =>
            Message.Response.make(~id=msg->Message.getId, ~result=Message.Result.null(), ())
            ->Message.Response.asMessage
            ->send
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

  // ////
  // BOOT
  // ////

  switch mode {
  | Stdio =>
    let writer = Rpc.StreamMessageWriter.make(stdout)
    let reader = Rpc.StreamMessageReader.make(stdin)
    sendFn := (msg => writer->Rpc.StreamMessageWriter.write(msg))
    reader->Rpc.StreamMessageReader.listen(onMessage)
    log(`Starting LSP in stdio mode.`)

  | NodeRpc =>
    sendFn := processSend
    processOnMessage(onMessage)
    log(`Starting LSP in Node RPC.`)
  }
}

external process: 'any = "process"

process
->Process.onUnhandledRejection((err, _p) => {
  Console.error(err)
})
->ignore
