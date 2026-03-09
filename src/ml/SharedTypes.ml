type path = string list

type typedFnArg = Asttypes.arg_label * Types.type_expr

module ModulePath = struct
  type t =
    | File of Uri.t * string
    | NotVisible
    | IncludedModule of Path.t * t
    | ExportedModule of {name: string; modulePath: t; isType: bool}

  let toPath modulePath tipName : path =
    let rec loop modulePath current =
      match modulePath with
      | File _ -> current
      | IncludedModule (_, inner) -> loop inner current
      | ExportedModule {name; modulePath = inner} -> loop inner (name :: current)
      | NotVisible -> current
    in
    loop modulePath [tipName]

  let toPathWithPrefix modulePath prefix : path =
    let rec loop modulePath current =
      match modulePath with
      | File _ -> current
      | IncludedModule (_, inner) -> loop inner current
      | ExportedModule {name; modulePath = inner} -> loop inner (name :: current)
      | NotVisible -> current
    in
    prefix :: loop modulePath []
end

type field = {
  stamp: int;
  fname: string Location.loc;
  typ: Types.type_expr;
  optional: bool;
  docstring: string list;
  deprecated: string option;
  attributes: Parsetree.attributes;
}

type constructorArgs =
  | InlineRecord of field list
  | Args of (Types.type_expr * Location.t) list

module Constructor = struct
  type t = {
    stamp: int;
    cname: string Location.loc;
    args: constructorArgs;
    res: Types.type_expr option;
    typeDecl: string * Types.type_declaration;
    docstring: string list;
    deprecated: string option;
    attributes: Parsetree.attributes;
  }
end

module Type = struct
  type kind =
    | Abstract of (Path.t * Types.type_expr list) option
    | Open
    | Tuple of Types.type_expr list
    | Record of field list
    | Variant of Constructor.t list

  type t = {
    kind: kind;
    decl: Types.type_declaration;
    name: string;
    attributes: Parsetree.attributes;
  }
end

module Exported = struct
  type namedStampMap = (string, int) Hashtbl.t

  type t = {
    types_: namedStampMap;
    values_: namedStampMap;
    modules_: namedStampMap;
  }

  type kind = Type | Value | Module

  let init () =
    {
      types_ = Hashtbl.create 10;
      values_ = Hashtbl.create 10;
      modules_ = Hashtbl.create 10;
    }

  let add t kind name x =
    let tbl =
      match kind with
      | Type -> t.types_
      | Value -> t.values_
      | Module -> t.modules_
    in
    if Hashtbl.mem tbl name then false
    else
      let () = Hashtbl.add tbl name x in
      true

  let find t kind name =
    let tbl =
      match kind with
      | Type -> t.types_
      | Value -> t.values_
      | Module -> t.modules_
    in
    Hashtbl.find_opt tbl name

  let iter t kind f =
    let tbl =
      match kind with
      | Type -> t.types_
      | Value -> t.values_
      | Module -> t.modules_
    in
    Hashtbl.iter f tbl
end

module Module = struct
  type kind =
    | Value of Types.type_expr
    | Type of Type.t * Types.rec_status
    | Module of {type_: t; isModuleType: bool}

  and item = {
    kind: kind;
    name: string;
    loc: Location.t;
    docstring: string list;
    deprecated: string option;
  }

  and structure = {
    name: string;
    docstring: string list;
    exported: Exported.t;
    items: item list;
    deprecated: string option;
  }

  and t = Ident of Path.t | Structure of structure | Constraint of t * t
end

module Declared = struct
  type 'item t = {
    name: string Location.loc;
    extentLoc: Location.t;
    stamp: int;
    modulePath: ModulePath.t;
    isExported: bool;
    deprecated: string option;
    docstring: string list;
    item: 'item;
  }
end

module Stamps : sig
  type kind =
    | KType of Type.t Declared.t
    | KValue of Types.type_expr Declared.t
    | KModule of Module.t Declared.t
    | KConstructor of Constructor.t Declared.t

  val locOfKind : kind -> Warnings.loc

  type t

  val addConstructor : t -> int -> Constructor.t Declared.t -> unit
  val addModule : t -> int -> Module.t Declared.t -> unit
  val addType : t -> int -> Type.t Declared.t -> unit
  val addValue : t -> int -> Types.type_expr Declared.t -> unit
  val findModule : t -> int -> Module.t Declared.t option
  val findType : t -> int -> Type.t Declared.t option
  val findValue : t -> int -> Types.type_expr Declared.t option
  val init : unit -> t
  val iterConstructors : (int -> Constructor.t Declared.t -> unit) -> t -> unit
  val iterModules : (int -> Module.t Declared.t -> unit) -> t -> unit
  val iterTypes : (int -> Type.t Declared.t -> unit) -> t -> unit
  val iterValues : (int -> Types.type_expr Declared.t -> unit) -> t -> unit
  val getEntries : t -> (int * kind) list
end = struct
  type 't stampMap = (int, 't Declared.t) Hashtbl.t

  type kind =
    | KType of Type.t Declared.t
    | KValue of Types.type_expr Declared.t
    | KModule of Module.t Declared.t
    | KConstructor of Constructor.t Declared.t

  let locOfKind = function
    | KType declared -> declared.extentLoc
    | KValue declared -> declared.extentLoc
    | KModule declared -> declared.extentLoc
    | KConstructor declared -> declared.extentLoc

  type t = (int, kind) Hashtbl.t

  let init () = Hashtbl.create 10

  let addConstructor (stamps : t) stamp declared =
    Hashtbl.add stamps stamp (KConstructor declared)

  let addModule stamps stamp declared =
    Hashtbl.add stamps stamp (KModule declared)

  let addType stamps stamp declared = Hashtbl.add stamps stamp (KType declared)

  let addValue stamps stamp declared =
    Hashtbl.add stamps stamp (KValue declared)

  let findModule stamps stamp =
    match Hashtbl.find_opt stamps stamp with
    | Some (KModule declared) -> Some declared
    | _ -> None

  let findType stamps stamp =
    match Hashtbl.find_opt stamps stamp with
    | Some (KType declared) -> Some declared
    | _ -> None

  let findValue stamps stamp =
    match Hashtbl.find_opt stamps stamp with
    | Some (KValue declared) -> Some declared
    | _ -> None

  let iterModules f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KModule d -> f stamp d
        | _ -> ())
      stamps

  let iterTypes f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KType d -> f stamp d
        | _ -> ())
      stamps

  let iterValues f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KValue d -> f stamp d
        | _ -> ())
      stamps

  let iterConstructors f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KConstructor d -> f stamp d
        | _ -> ())
      stamps

  let getEntries t = t |> Hashtbl.to_seq |> List.of_seq
end

module File = struct
  type t = {
    uri: Uri.t;
    stamps: Stamps.t;
    moduleName: string;
    structure: Module.structure;
  }

  let create moduleName uri =
    {
      uri;
      stamps = Stamps.init ();
      moduleName;
      structure =
        {
          name = moduleName;
          docstring = [];
          exported = Exported.init ();
          items = [];
          deprecated = None;
        };
    }
end

module QueryEnv : sig
  type t = private {
    file: File.t;
    exported: Exported.t;
    pathRev: path;
    parent: t option;
  }
  val fromFile : File.t -> t
  val enterStructure : t -> Module.structure -> t

  (* Express a path starting from the module represented by the env.
     E.g. the env is at A.B.C and the path is D.
     The result is A.B.C.D if D is inside C.
     Or A.B.D or A.D or D if it's in one of its parents. *)
  val pathFromEnv : t -> path -> bool * path

  val toString : t -> string
end = struct
  type t = {file: File.t; exported: Exported.t; pathRev: path; parent: t option}

  let toString {file; pathRev} =
    file.moduleName :: List.rev pathRev |> String.concat "."

  let fromFile (file : File.t) =
    {file; exported = file.structure.exported; pathRev = []; parent = None}

  (* Prune a path and find a parent environment that contains the module name *)
  let rec prunePath pathRev env name =
    if Exported.find env.exported Module name <> None then (true, pathRev)
    else
      match (pathRev, env.parent) with
      | _ :: rest, Some env -> prunePath rest env name
      | _ -> (false, [])

  let pathFromEnv env path =
    match path with
    | [] -> (true, env.pathRev |> List.rev)
    | name :: _ ->
      let found, prunedPathRev = prunePath env.pathRev env name in
      (found, List.rev_append prunedPathRev path)

  let enterStructure env (structure : Module.structure) =
    let name = structure.name in
    let pathRev = name :: snd (prunePath env.pathRev env name) in
    {env with exported = structure.exported; pathRev; parent = Some env}
end

module Env = struct
  type t = {stamps: Stamps.t; modulePath: ModulePath.t}
  let addExportedModule ~name ~isType env =
    {
      env with
      modulePath = ExportedModule {name; modulePath = env.modulePath; isType};
    }
  let addModule ~name env = env |> addExportedModule ~name ~isType:false
  let addModuleType ~name env = env |> addExportedModule ~name ~isType:true
end

type filePath = string

type paths =
  | Impl of {cmt: filePath; res: filePath}
  | Namespace of {cmt: filePath}
  | IntfAndImpl of {
      cmti: filePath;
      resi: filePath;
      cmt: filePath;
      res: filePath;
    }

let getSrc p =
  match p with
  | Impl {res} -> [res]
  | Namespace _ -> []
  | IntfAndImpl {resi; res} -> [resi; res]

let getUri p =
  match p with
  | Impl {res} -> Uri.fromPath res
  | Namespace {cmt} -> Uri.fromPath cmt
  | IntfAndImpl {resi} -> Uri.fromPath resi

let getUris p =
  match p with
  | Impl {res} -> [Uri.fromPath res]
  | Namespace {cmt} -> [Uri.fromPath cmt]
  | IntfAndImpl {res; resi} -> [Uri.fromPath res; Uri.fromPath resi]

let getCmtPath ~uri p =
  match p with
  | Impl {cmt} -> cmt
  | Namespace {cmt} -> cmt
  | IntfAndImpl {cmti; cmt} ->
    let interface = Utils.endsWith (Uri.toPath uri) "i" in
    if interface then cmti else cmt

type file = string

module FileSet = Set.Make (String)

type package = {
  genericJsxModule: string option;
  suffix: string;
  rootPath: filePath;
  projectFiles: FileSet.t;
  dependenciesFiles: FileSet.t;
  pathsForModule: (file, paths) Hashtbl.t;
  namespace: string option;
  opens: path list;
  uncurried: bool;
  rescriptVersion: int * int;
  autocomplete: file list Misc.StringMap.t;
}

let allFilesInPackage package =
  FileSet.union package.projectFiles package.dependenciesFiles

type full = {file: File.t; package: package}

type state = {
  packagesByRoot: (string, package) Hashtbl.t;
  rootForUri: (Uri.t, string) Hashtbl.t;
}

(* There's only one state, so it can as well be global *)
let state = {packagesByRoot = Hashtbl.create 1; rootForUri = Hashtbl.create 30}
