open GenerateSchemaTypes
open GenerateSchemaDiagnostics

(*
  This aims to implement the most important validations from graphql-js directly
  in ResGraph. We're fine with letting some errors through, having graphql-js
  report them at runtime instead. But the large bulk of errors you'd normally
  encounter should be reimplemented here, so the DX of ResGraph is good enough.
*)

let emptyLoc =
  {
    Location.loc_start = Lexing.dummy_pos;
    loc_end = Lexing.dummy_pos;
    loc_ghost = true;
  }

let mkTypeLocation ~typeName ~fileName ~fileUri ~loc =
  Concrete {fileName; fileUri; modulePath = []; typeName; loc}

let validateName ~name ~(typeLocation : typeLocation)
    (schemaState : schemaState) =
  match typeLocation with
  | Synthetic _ -> ()
  | Concrete typeLocation ->
    if Utils.startsWith name "__" then
      schemaState
      |> addDiagnostic
           ~diagnostic:
             {
               loc = typeLocation.loc;
               fileUri = typeLocation.fileUri;
               message =
                 Printf.sprintf
                   "Name \"%s\" must not begin with \"__\", which is reserved \
                    by GraphQL introspection."
                   name;
             }

let validateFields ~schemaState (fields : gqlField list) =
  fields
  |> List.iter (fun (f : gqlField) ->
         validateName ~name:f.name
           ~typeLocation:
             (mkTypeLocation ~typeName:f.name ~loc:f.loc ~fileName:f.fileName
                ~fileUri:f.fileUri)
           schemaState)

let validateRootTypes (schemaState : schemaState) =
  match schemaState.query with
  | None ->
    schemaState
    |> addDiagnostic
         ~diagnostic:
           {
             loc = emptyLoc;
             fileUri = Uri.fromPath "<root>";
             message = "You must define at least a `query` type in your schema.";
           }
  | Some _ -> ()

let validateSchema (schemaState : schemaState) =
  validateRootTypes schemaState;

  schemaState.scalars
  |> Hashtbl.iter (fun _name (typ : gqlScalar) ->
         validateName ~name:typ.displayName
           ~typeLocation:(Concrete typ.typeLocation) schemaState);

  schemaState.types
  |> Hashtbl.iter (fun _name (typ : gqlObjectType) ->
         match typ.typeLocation with
         | Some _typeLocation -> validateFields ~schemaState typ.fields
         | None -> ());

  schemaState.inputObjects
  |> Hashtbl.iter (fun _name (typ : gqlInputObjectType) ->
         (* A lot has already been validated on adding the type itself. *)
         match typ.typeLocation with
         | Some _typeLocation -> validateFields ~schemaState typ.fields
         | None -> ());

  schemaState.enums
  |> Hashtbl.iter (fun _name (typ : gqlEnum) ->
         (* No need to validate each case, ReScript has already done it for us. *)
         validateName ~name:typ.displayName ~typeLocation:typ.typeLocation
           schemaState);

  schemaState.unions
  |> Hashtbl.iter (fun _name (typ : gqlUnion) ->
         (* No need to validate each case, ReScript has already done it for us. *)
         validateName ~name:typ.displayName ~typeLocation:typ.typeLocation
           schemaState);

  schemaState.interfaces
  |> Hashtbl.iter (fun _name (typ : gqlInterface) ->
         (* Subtype rules etc for interface fields are a bit complicated, so we
            let graphql-js do it at runtime instead. *)
         validateFields ~schemaState typ.fields)
