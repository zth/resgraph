type t = {
  moduleName: string;
  uri: Uri.t;
  infos: Cmt_format.cmt_infos;
}

let of_path ~moduleName ~path =
  match Shared.tryReadCmt path with
  | None -> None
  | Some infos ->
    let uri = Uri.fromPath path in
    Some {moduleName; uri; infos}

let module_name t = t.moduleName
let uri t = t.uri
let infos t = t.infos

let structure_items t =
  match t.infos.cmt_annots with
  | Implementation structure -> Some structure.str_items
  | Partial_implementation parts ->
    parts
    |> Array.to_list
    |> Utils.filterMap (function
         | Cmt_format.Partial_structure str -> Some str.str_items
         | Partial_structure_item str -> Some [str]
         | _ -> None)
    |> List.concat |> Option.some
  | _ -> None

let signature_items t =
  match t.infos.cmt_annots with
  | Interface signature -> Some signature.sig_items
  | Partial_interface parts ->
    parts
    |> Array.to_list
    |> Utils.filterMap (function
         | Cmt_format.Partial_signature str -> Some str.sig_items
         | Partial_signature_item str -> Some [str]
         | _ -> None)
    |> List.concat |> Option.some
  | _ -> None
