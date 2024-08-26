"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[252],{3905:(e,n,t)=>{t.d(n,{Zo:()=>p,kt:()=>h});var a=t(7294);function r(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function i(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function o(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?i(Object(t),!0).forEach((function(n){r(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):i(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function s(e,n){if(null==e)return{};var t,a,r=function(e,n){if(null==e)return{};var t,a,r={},i=Object.keys(e);for(a=0;a<i.length;a++)t=i[a],n.indexOf(t)>=0||(r[t]=e[t]);return r}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)t=i[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(r[t]=e[t])}return r}var l=a.createContext({}),d=function(e){var n=a.useContext(l),t=n;return e&&(t="function"==typeof e?e(n):o(o({},n),e)),t},p=function(e){var n=d(e.components);return a.createElement(l.Provider,{value:n},e.children)},u="mdxType",m={inlineCode:"code",wrapper:function(e){var n=e.children;return a.createElement(a.Fragment,{},n)}},c=a.forwardRef((function(e,n){var t=e.components,r=e.mdxType,i=e.originalType,l=e.parentName,p=s(e,["components","mdxType","originalType","parentName"]),u=d(t),c=r,h=u["".concat(l,".").concat(c)]||u[c]||m[c]||i;return t?a.createElement(h,o(o({ref:n},p),{},{components:t})):a.createElement(h,o({ref:n},p))}));function h(e,n){var t=arguments,r=n&&n.mdxType;if("string"==typeof e||r){var i=t.length,o=new Array(i);o[0]=c;var s={};for(var l in n)hasOwnProperty.call(n,l)&&(s[l]=n[l]);s.originalType=e,s[u]="string"==typeof e?e:r,o[1]=s;for(var d=2;d<i;d++)o[d]=t[d];return a.createElement.apply(null,o)}return a.createElement.apply(null,t)}c.displayName="MDXCreateElement"},7918:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>l,contentTitle:()=>o,default:()=>m,frontMatter:()=>i,metadata:()=>s,toc:()=>d});var a=t(7462),r=(t(7294),t(3905));const i={sidebar_position:4},o="Unions",s={unversionedId:"unions",id:"unions",title:"Unions",description:"You define a union by defining a variant with payloads of types tagged with @gql.type, and annotate that variant with @gql.union:",source:"@site/docs/unions.md",sourceDirName:".",slug:"/unions",permalink:"/resgraph/docs/unions",draft:!1,editUrl:"https://github.com/zth/resgraph/tree/main/docs/templates/shared/docs/unions.md",tags:[],version:"current",sidebarPosition:4,frontMatter:{sidebar_position:4},sidebar:"tutorialSidebar",previous:{title:"Enums",permalink:"/resgraph/docs/enums"},next:{title:"Interfaces",permalink:"/resgraph/docs/interfaces"}},l={},d=[{value:"Inline records for defining union members",id:"inline-records-for-defining-union-members",level:3},{value:"Using unions in the schema",id:"using-unions-in-the-schema",level:2},{value:"Advanced: Inferred unions",id:"advanced-inferred-unions",level:2},{value:"Inferring full union members",id:"inferring-full-union-members",level:3},{value:"Going beyond: More inference!",id:"going-beyond-more-inference",level:4},{value:"Sometimes it makes more sense to not use inferred unions",id:"sometimes-it-makes-more-sense-to-not-use-inferred-unions",level:3},{value:"Next steps",id:"next-steps",level:2}],p={toc:d},u="wrapper";function m(e){let{components:n,...t}=e;return(0,r.kt)(u,(0,a.Z)({},p,t,{components:n,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"unions"},"Unions"),(0,r.kt)("p",null,"You define a union by defining a variant with payloads of types tagged with ",(0,r.kt)("inlineCode",{parentName:"p"},"@gql.type"),", and annotate that variant with ",(0,r.kt)("inlineCode",{parentName:"p"},"@gql.union"),":"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},"@gql.type\ntype user = {\n  @gql.field name: string,\n  @gql.field age: int\n}\n\n@gql.type\ntype group = {\n  @gql.field displayName: string,\n}\n\n@gql.union\ntype entity = User(user) | Group(group)\n")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},"type User {\n  name: String!\n  age: Int!\n}\n\ntype Group {\n  displayName: String!\n}\n\nunion Entity = User | Group\n")),(0,r.kt)("p",null,"Each variant case can be called whatever you want it to (although it's good practice to follow the name of the GraphQL type it holds), but rememeber that the payload of each union variant case must be one of:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Exactly 1 type that has a ",(0,r.kt)("inlineCode",{parentName:"li"},"@gql.type")," annotation."),(0,r.kt)("li",{parentName:"ul"},"An inline record.")),(0,r.kt)("p",null,"Don't worry, ResGraph will complain if you try anything else."),(0,r.kt)("p",null,"You can add comments on the union type definition itself, as well as each variant case, and they'll end up in the schema."),(0,r.kt)("h3",{id:"inline-records-for-defining-union-members"},"Inline records for defining union members"),(0,r.kt)("p",null,'You can create ad hoc "synthetic" object types for your unions by using an ',(0,r.kt)("em",{parentName:"p"},"inline record")," as the payload for a variant case in union. Let's look at an example:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},"@gql.union\ntype setPasswordPayload = Ok({affectedUser: user}) | Failed({reason: string})\n")),(0,r.kt)("p",null,"This defines a variant called ",(0,r.kt)("inlineCode",{parentName:"p"},"setPasswordPayload")," with two cases - ",(0,r.kt)("inlineCode",{parentName:"p"},"Ok")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"Failed"),". Each of those cases also has additional fields through an inline record. The above will generate the following GraphQL:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},"union SetPasswordPayload = SetPasswordPayloadOk | SetPasswordPayloadFailed\n\ntype SetPasswordPayloadFailed {\n  reason: String!\n}\n\ntype SetPasswordPayloadOk {\n  affectedUser: User!\n}\n")),(0,r.kt)("p",null,"Notice a few things:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Each inline record has been synthesized into an actual GraphQL type holding ",(0,r.kt)("em",{parentName:"li"},"all")," fields that were defined in that inline record."),(0,r.kt)("li",{parentName:"ul"},"The synthetiszed GraphQL types are called ",(0,r.kt)("inlineCode",{parentName:"li"},"<unionName><caseName>"),". ",(0,r.kt)("inlineCode",{parentName:"li"},"Ok")," therefore becomes ",(0,r.kt)("inlineCode",{parentName:"li"},"SetPasswordPayloadOk"),".")),(0,r.kt)("p",null,"This is intended to be a quick way to define one-off GraphQL types only intended to be used in a specific enum, like how you'd typically design a result from a mutation."),(0,r.kt)("h2",{id:"using-unions-in-the-schema"},"Using unions in the schema"),(0,r.kt)("p",null,"Unions can be used as the type for fields on GraphQL objects or interfaces. A simple example:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},"@gql.type\ntype user = {\n  @gql.field name: string,\n  @gql.field age: int\n}\n\n@gql.type\ntype group = {\n  @gql.field displayName: string,\n}\n\n@gql.union\ntype entity = User(user) | Group(group)\n\n@gql.field\nlet entity = async (_: query, ~entityId, ~ctx: ResGraphContext.context): option<entity> => {\n  switch decodeEntityId(entityId) {\n  | Some(#User, id) =>\n    switch await ctx.dataLoaders.userById(~userId=id) {\n    | None => None\n    | Some(user) => Some(User(user))\n    }\n  | Some(#Group, id) =>\n    switch await ctx.dataLoaders.groupById(~groupId=id) {\n    | None => None\n    | Some(group) => Some(Group(group))\n    }\n  | _ => None\n  }\n}\n\n")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},"type User {\n  name: String!\n  age: Int!\n}\n\ntype Group {\n  displayName: String!\n}\n\nunion Entity = User | Group\n\ntype Query {\n  entity(entityId: String!): Entity\n}\n")),(0,r.kt)("h2",{id:"advanced-inferred-unions"},"Advanced: Inferred unions"),(0,r.kt)("p",null,"ResGraph can ",(0,r.kt)("em",{parentName:"p"},"infer")," unions from ",(0,r.kt)("a",{parentName:"p",href:"https://rescript-lang.org/docs/manual/latest/polymorphic-variant"},"polymorphic variants")," in ReScript. This can be very useful when protoyping and working with ResGraph, and can speed you up considerably - get unions without having to declare them."),(0,r.kt)("p",null,"Let's look at an example:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},"let bestFriend = (user: user) => {\n  switch await ctx.db.user.bestFriend(~userId) {\n  | Ok(User(user)) => Some(#User(user))\n  | Ok(Dog(dog)) => Some(#Dog(dog))\n  | Error(_) => None\n  }\n}\n")),(0,r.kt)("p",null,"This produces this schema:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},"union UserBestFriend = User | Dog\n\ntype User {\n  bestFriend: UserBestFriend\n}\n")),(0,r.kt)("p",null,"It infers that you're returning either a ",(0,r.kt)("inlineCode",{parentName:"p"},"User")," or a ",(0,r.kt)("inlineCode",{parentName:"p"},"Dog")," by looking at the type ",(0,r.kt)("em",{parentName:"p"},"inside")," of the polyvariant you return. It then creates a union automatically for that, named after the type + field name it was found on."),(0,r.kt)("p",null,"This is neat! This means you can quickly produce a union without having to declare them beforehand."),(0,r.kt)("h3",{id:"inferring-full-union-members"},"Inferring full union members"),(0,r.kt)("p",null,"However, to make this really nice, we can combine this with ",(0,r.kt)("em",{parentName:"p"},"inferring full objects in union members")," to very easily add ah hoc unions. This is particularly efficient in mutations. Example:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},'let userUpdateName = (\n  _: mutation,\n  ~userId: ResGraph.id,\n  ~newName,\n  ~ctx: ResGraphContext.context,\n) => {\n  switch await ctx.db.updateUserName(\n    ~userId=ResGraph.idToString(userId),\n    ~name=newName,\n  ) {\n  | Ok(updatedUser) => Some(#UserNameWasUpdated({"updatedUser": updatedUser}))\n  | Error(Unauthorized) =>\n    Some(#UserNameUpdateFailed({"message": "Not authorized."}))\n  | Error(_) =>\n    Some(#UserNameUpdateFailed({"message": "Something went wrong."}))\n  }\n}\n\n')),(0,r.kt)("p",null,"This produces this schema:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},"type UserNameWasUpdated {\n  updatedUser: User!\n}\n\ntype UserNameUpdateFailed {\n  message: String!\n}\n\nunion UserUpdateName = UserNameWasUpdated | UserNameUpdateFailed\n\ntype Mutation {\n  userUpdateName(userId: ID!, newName: String!): UserUpdateName\n}\n")),(0,r.kt)("p",null,"Pretty neat. We now have an ad hoc union for our mutation with 0 boilerplate."),(0,r.kt)("h4",{id:"going-beyond-more-inference"},"Going beyond: More inference!"),(0,r.kt)("p",null,"As a little side note, we can extend this even further with more inference. Let's add an inferred enum as well to our result type instead of a message string:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},'let userUpdateName = (\n  _: mutation,\n  ~userId: ResGraph.id,\n  ~newName,\n  ~ctx: ResGraphContext.context,\n) => {\n  switch await ctx.db.updateUserName(\n    ~userId=ResGraph.idToString(userId),\n    ~name=newName,\n  ) {\n  | Ok(updatedUser) => Some(#UserNameWasUpdated({"updatedUser": updatedUser}))\n  | Error(Unauthorized) =>\n    Some(#UserNameUpdateFailed({"reason": #NOT_AUTHORIZED}))\n  | Error(_) => Some(#UserNameUpdateFailed({"reason": #UNKNOWN_ERROR}))\n  }\n}\n\n')),(0,r.kt)("p",null,"This produces this schema:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},"enum UserNameUpdateFailedReason {\n  NOT_AUTHORIZED\n  UNKNOWN_ERROR\n}\n\ntype UserNameWasUpdated {\n  updatedUser: User!\n}\n\ntype UserNameUpdateFailed {\n  reason: UserNameUpdateFailedReason!\n}\n\nunion UserUpdateName = UserNameWasUpdated | UserNameUpdateFailed\n\ntype Mutation {\n  userUpdateName(userId: ID!, newName: String!): UserUpdateName\n}\n")),(0,r.kt)("p",null,"There, we now have an inferred enum as well inside of our inferred union. Inference everywhere!"),(0,r.kt)("h3",{id:"sometimes-it-makes-more-sense-to-not-use-inferred-unions"},"Sometimes it makes more sense to not use inferred unions"),(0,r.kt)("p",null,"Inferred unions can be used as return types from resolvers only. Each union will be named ",(0,r.kt)("inlineCode",{parentName:"p"},"<parentTypeName><fieldName>"),", and optionally if it's on a mutation, be appended with ",(0,r.kt)("inlineCode",{parentName:"p"},"Result"),"."),(0,r.kt)("p",null,"Inferred unions certainly has their place, especially in mutations. But just as with inferred enums, there are downsides. They:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Can't use doc strings"),(0,r.kt)("li",{parentName:"ul"},"Can't control the name of the generated type"),(0,r.kt)("li",{parentName:"ul"},"Can't reuse the type unless you declare it")),(0,r.kt)("p",null,"Regardless of the downsides, inferred unions are really cool and useful. Use them when they make sense!"),(0,r.kt)("h2",{id:"next-steps"},"Next steps"),(0,r.kt)("p",null,"Now that we've covered unions, we can move on to ",(0,r.kt)("a",{parentName:"p",href:"interfaces"},"interfaces"),"."))}m.isMDXComponent=!0}}]);