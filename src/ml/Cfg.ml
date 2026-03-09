let isDocGenFromCompiler = ref false

let readProjectConfigCache =
  ref
    (try
       match Sys.getenv "RESCRIPT_PROJECT_CONFIG_CACHE" with
       | "true" -> true
       | _ -> false
     with _ -> false)
