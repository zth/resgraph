open Interface_labelled

let dummyQuery: Query.query = Obj.magic(())

let scalarValue: AppCustomScalars.scalarHolder =
  AppCustomScalars.getScalarHolder(dummyQuery)
// ^hov

let typenameFn: AppLabelledTypes.labelled => string =
  AppInterfaceExtras.typenameEcho(_, ~typeName=Interface_labelled.ImplementedBy.LabelledAlpha)
// ^hov
