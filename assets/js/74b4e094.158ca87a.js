"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[252],{3905:(e,n,t)=>{t.d(n,{Zo:()=>d,kt:()=>m});var a=t(7294);function r(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function i(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function o(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?i(Object(t),!0).forEach((function(n){r(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):i(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function l(e,n){if(null==e)return{};var t,a,r=function(e,n){if(null==e)return{};var t,a,r={},i=Object.keys(e);for(a=0;a<i.length;a++)t=i[a],n.indexOf(t)>=0||(r[t]=e[t]);return r}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)t=i[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(r[t]=e[t])}return r}var s=a.createContext({}),p=function(e){var n=a.useContext(s),t=n;return e&&(t="function"==typeof e?e(n):o(o({},n),e)),t},d=function(e){var n=p(e.components);return a.createElement(s.Provider,{value:n},e.children)},u="mdxType",c={inlineCode:"code",wrapper:function(e){var n=e.children;return a.createElement(a.Fragment,{},n)}},y=a.forwardRef((function(e,n){var t=e.components,r=e.mdxType,i=e.originalType,s=e.parentName,d=l(e,["components","mdxType","originalType","parentName"]),u=p(t),y=r,m=u["".concat(s,".").concat(y)]||u[y]||c[y]||i;return t?a.createElement(m,o(o({ref:n},d),{},{components:t})):a.createElement(m,o({ref:n},d))}));function m(e,n){var t=arguments,r=n&&n.mdxType;if("string"==typeof e||r){var i=t.length,o=new Array(i);o[0]=y;var l={};for(var s in n)hasOwnProperty.call(n,s)&&(l[s]=n[s]);l.originalType=e,l[u]="string"==typeof e?e:r,o[1]=l;for(var p=2;p<i;p++)o[p]=t[p];return a.createElement.apply(null,o)}return a.createElement.apply(null,t)}y.displayName="MDXCreateElement"},7918:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>s,contentTitle:()=>o,default:()=>c,frontMatter:()=>i,metadata:()=>l,toc:()=>p});var a=t(7462),r=(t(7294),t(3905));const i={sidebar_position:4},o="Unions",l={unversionedId:"unions",id:"unions",title:"Unions",description:"You define a union by defining a variant with payloads of types tagged with @gql.type, and annotate that variant with @gql.union:",source:"@site/docs/unions.md",sourceDirName:".",slug:"/unions",permalink:"/resgraph/docs/unions",draft:!1,editUrl:"https://github.com/zth/resgraph/tree/main/docs/templates/shared/docs/unions.md",tags:[],version:"current",sidebarPosition:4,frontMatter:{sidebar_position:4},sidebar:"tutorialSidebar",previous:{title:"Enums",permalink:"/resgraph/docs/enums"},next:{title:"Interfaces",permalink:"/resgraph/docs/interfaces"}},s={},p=[{value:"Inline records for defining union members",id:"inline-records-for-defining-union-members",level:3},{value:"Using unions in the schema",id:"using-unions-in-the-schema",level:2}],d={toc:p},u="wrapper";function c(e){let{components:n,...t}=e;return(0,r.kt)(u,(0,a.Z)({},d,t,{components:n,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"unions"},"Unions"),(0,r.kt)("p",null,"You define a union by defining a variant with payloads of types tagged with ",(0,r.kt)("inlineCode",{parentName:"p"},"@gql.type"),", and annotate that variant with ",(0,r.kt)("inlineCode",{parentName:"p"},"@gql.union"),":"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},"@gql.type\ntype user = {\n  @gql.field name: string,\n  @gql.field age: int\n}\n\n@gql.type\ntype group = {\n  @gql.field displayName: string,\n}\n\n@gql.union\ntype entity = User(user) | Group(group)\n")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},"type User {\n  name: String!\n  age: Int!\n}\n\ntype Group {\n  displayName: String!\n}\n\nunion Entity = User | Group\n")),(0,r.kt)("p",null,"Each variant case can be called whatever you want it to (although it's good practice to follow the name of the GraphQL type it holds), but rememeber that the payload of each union variant case must be one of:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Exactly 1 type that has a ",(0,r.kt)("inlineCode",{parentName:"li"},"@gql.type")," annotation"),(0,r.kt)("li",{parentName:"ul"},"An inline record")),(0,r.kt)("p",null,"But don't worry, ResGraph will complain if you try anything else."),(0,r.kt)("p",null,"You can add comments on the union type definition itself, as well as each variant case, and they'll end up in the schema."),(0,r.kt)("h3",{id:"inline-records-for-defining-union-members"},"Inline records for defining union members"),(0,r.kt)("p",null,'You can create ad hoc "synthetic" object types for your unions by using an ',(0,r.kt)("em",{parentName:"p"},"inline record")," as the payload for a variant case in union. Let's look at an example:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},"@gql.union\ntype setPasswordPayload = Ok({affectedUser: user}) | Failed({reason: string})\n")),(0,r.kt)("p",null,"This defines a variant called ",(0,r.kt)("inlineCode",{parentName:"p"},"setPasswordPayload")," with two cases - ",(0,r.kt)("inlineCode",{parentName:"p"},"Ok")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"Failed"),". Each of those cases also has additional fields through an inline record. The above will generate the following GraphQL:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},"union SetPasswordPayload\n{\n  SetPasswordPayloadOk\n  SetPasswordPayloadFailed\n}\n\ntype SetPasswordPayloadFailed {\n  reason: String!\n}\n\ntype SetPasswordPayloadOk {\n  affectedUser: User!\n}\n")),(0,r.kt)("p",null,"Notice a few things:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Each inline record has been synthesized into an actual GraphQL type holding ",(0,r.kt)("em",{parentName:"li"},"all")," fields that were defined in that inline record."),(0,r.kt)("li",{parentName:"ul"},"The synthetiszed GraphQL types are called ",(0,r.kt)("inlineCode",{parentName:"li"},"<unionName><caseName>"),". ",(0,r.kt)("inlineCode",{parentName:"li"},"Ok")," therefore becomes ",(0,r.kt)("inlineCode",{parentName:"li"},"SetPasswordPayloadOk"),".")),(0,r.kt)("p",null,"This is intended to be a quick way to define one-off GraphQL types only intended to be used in a specific enum, like how you'd typically design a result from a mutation."),(0,r.kt)("h2",{id:"using-unions-in-the-schema"},"Using unions in the schema"),(0,r.kt)("p",null,"Unions can be used as the type for fields on GraphQL objects or interfaces. A simple example:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},"@gql.type\ntype user = {\n  @gql.field name: string,\n  @gql.field age: int\n}\n\n@gql.type\ntype group = {\n  @gql.field displayName: string,\n}\n\n@gql.union\ntype entity = User(user) | Group(group)\n\n@gql.field\nlet entity = async (_: query, ~entityId, ~ctx: ResGraphContext.context): option<entity> => {\n  switch decodeEntityId(entityId) {\n  | Some(#User, id) =>\n    switch await ctx.dataLoaders.userById(~userId=id) {\n    | None => None\n    | Some(user) => Some(User(user))\n    }\n  | Some(#Group, id) =>\n    switch await ctx.dataLoaders.groupById(~groupId=id) {\n    | None => None\n    | Some(group) => Some(Group(group))\n    }\n  | _ => None\n  }\n}\n\n")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},"type User {\n  name: String!\n  age: Int!\n}\n\ntype Group {\n  displayName: String!\n}\n\nunion Entity = User | Group\n\ntype Query {\n  entity(entityId: String!): Entity\n}\n")),(0,r.kt)("p",null,"Now that we've covered unions, we can move on to ",(0,r.kt)("a",{parentName:"p",href:"interfaces"},"interfaces"),"."))}c.isMDXComponent=!0}}]);