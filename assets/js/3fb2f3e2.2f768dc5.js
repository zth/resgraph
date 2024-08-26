"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[340],{3905:(e,n,t)=>{t.d(n,{Zo:()=>p,kt:()=>h});var a=t(7294);function r(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function s(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function i(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?s(Object(t),!0).forEach((function(n){r(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):s(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function o(e,n){if(null==e)return{};var t,a,r=function(e,n){if(null==e)return{};var t,a,r={},s=Object.keys(e);for(a=0;a<s.length;a++)t=s[a],n.indexOf(t)>=0||(r[t]=e[t]);return r}(e,n);if(Object.getOwnPropertySymbols){var s=Object.getOwnPropertySymbols(e);for(a=0;a<s.length;a++)t=s[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(r[t]=e[t])}return r}var l=a.createContext({}),u=function(e){var n=a.useContext(l),t=n;return e&&(t="function"==typeof e?e(n):i(i({},n),e)),t},p=function(e){var n=u(e.components);return a.createElement(l.Provider,{value:n},e.children)},m="mdxType",c={inlineCode:"code",wrapper:function(e){var n=e.children;return a.createElement(a.Fragment,{},n)}},d=a.forwardRef((function(e,n){var t=e.components,r=e.mdxType,s=e.originalType,l=e.parentName,p=o(e,["components","mdxType","originalType","parentName"]),m=u(t),d=r,h=m["".concat(l,".").concat(d)]||m[d]||c[d]||s;return t?a.createElement(h,i(i({ref:n},p),{},{components:t})):a.createElement(h,i({ref:n},p))}));function h(e,n){var t=arguments,r=n&&n.mdxType;if("string"==typeof e||r){var s=t.length,i=new Array(s);i[0]=d;var o={};for(var l in n)hasOwnProperty.call(n,l)&&(o[l]=n[l]);o.originalType=e,o[m]="string"==typeof e?e:r,i[1]=o;for(var u=2;u<s;u++)i[u]=t[u];return a.createElement.apply(null,i)}return a.createElement.apply(null,t)}d.displayName="MDXCreateElement"},2294:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>l,contentTitle:()=>i,default:()=>c,frontMatter:()=>s,metadata:()=>o,toc:()=>u});var a=t(7462),r=(t(7294),t(3905));const s={sidebar_position:3},i="Enums",o={unversionedId:"enums",id:"enums",title:"Enums",description:"Enums are defined by defining a variant without payloads and annotate it with @gql.enum:",source:"@site/docs/enums.md",sourceDirName:".",slug:"/enums",permalink:"/resgraph/docs/enums",draft:!1,editUrl:"https://github.com/zth/resgraph/tree/main/docs/templates/shared/docs/enums.md",tags:[],version:"current",sidebarPosition:3,frontMatter:{sidebar_position:3},sidebar:"tutorialSidebar",previous:{title:"Object Types",permalink:"/resgraph/docs/object-types"},next:{title:"Unions",permalink:"/resgraph/docs/unions"}},l={},u=[{value:"Using enums in the schema",id:"using-enums-in-the-schema",level:2},{value:"Comments and deprecations",id:"comments-and-deprecations",level:2},{value:"Customizing enum case values",id:"customizing-enum-case-values",level:2},{value:"Advanced: Inferred enums",id:"advanced-inferred-enums",level:2},{value:"Next steps",id:"next-steps",level:2}],p={toc:u},m="wrapper";function c(e){let{components:n,...t}=e;return(0,r.kt)(m,(0,a.Z)({},p,t,{components:n,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"enums"},"Enums"),(0,r.kt)("p",null,"Enums are defined by defining a variant without payloads and annotate it with ",(0,r.kt)("inlineCode",{parentName:"p"},"@gql.enum"),":"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},"@gql.enum\ntype userStatus = Online | Offline\n")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},"enum UserStatus {\n  Online\n  Offline\n}\n")),(0,r.kt)("p",null,"Notice enums ",(0,r.kt)("em",{parentName:"p"},"must")," be variants without payloads, or ResGraph will complain."),(0,r.kt)("h2",{id:"using-enums-in-the-schema"},"Using enums in the schema"),(0,r.kt)("p",null,"Enums are valid to use anywhere in your schema where they are valid in GraphQL. ResGraph just needs to understand that it's your particular enum it's looking for. A few examples:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},"@gql.enum\ntype userStatus = Online | Offline\n\n@gql.type\ntype user = {\n  id: string,\n\n  @gql.field\n  currentStatus: option<userStatus>\n}\n\n@gql.field\nlet lastKnownStatus = (user: user, ~ctx: ResGraphContext.context) => {\n  // Returns promise<option<userStatus>>\n  ctx.dataLoaders.user.lastKnownStatus.load(~userId=user.id)\n}\n\n@gql.field\nlet currentUsersWithStatus = (_: query, ~status: userStatus, ~ctx: ResGraphContext.context) => {\n  // Returns promise<array<user>>\n  ctx.dataLoaders.user.byCurrentStatus.load(~status)\n}\n")),(0,r.kt)("p",null,"This above will generate the following GraphQL:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},"enum UserStatus {\n  Online\n  Offline\n}\n\ntype User {\n  currentStatus: UserStatus\n  lastKnownStatus: UserStatus\n}\n\ntype Query {\n  currentUsersWithStatus: [User!]!\n}\n")),(0,r.kt)("h2",{id:"comments-and-deprecations"},"Comments and deprecations"),(0,r.kt)("p",null,"You can add comments to the enum definition, and to each enum case. You can also add deprecations to each enum case via the ",(0,r.kt)("inlineCode",{parentName:"p"},"@deprecated")," attribute. Here's a full example demonstrating all of the above:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},'/** The status of the user.*/\n@gql.enum\ntype userStatus =\n  | /** The user is online. */ Online\n  | /** The user is offline. */ Offline\n  | /** The user is idle. */ @deprecated("This is going away") Idle\n')),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},'"""\nThe status of the user.\n"""\nenum UserStatus {\n  """\n  The user is online.\n  """\n  Online\n  """\n  The user is offline.\n  """\n  Offline\n  """\n  The user is idle.\n  """\n  Idle @deprecated(reason: "This is going away")\n}\n')),(0,r.kt)("h2",{id:"customizing-enum-case-values"},"Customizing enum case values"),(0,r.kt)("p",null,"Sometimes you want to customize the value of an enum case for various reasons, in a way that might not be possible given ReScript's rules around naming variant cases. For that, you can use the ",(0,r.kt)("inlineCode",{parentName:"p"},"@as")," attribute together with a string literal:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},'@gql.enum\ntype userStatus =\n  |\xa0@as("ONLINE") Online\n  | @as("OFFLINE") Offline\n')),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},"enum UserStatus {\n  ONLINE\n  OFFLINE\n}\n")),(0,r.kt)("h2",{id:"advanced-inferred-enums"},"Advanced: Inferred enums"),(0,r.kt)("p",null,"The strategies outlined above are the most efficient and best ways of using enums in ResGraph. However, there are scenarios, like when prototyping or working on something isolated that won't be reused, where you might now want to have to write out your enum type definition by hand. In these cases, you can ",(0,r.kt)("em",{parentName:"p"},"infer your enums from polyvariants"),"."),(0,r.kt)("p",null,"Remember ",(0,r.kt)("a",{parentName:"p",href:"https://rescript-lang.org/docs/manual/latest/polymorphic-variant"},"polymorphic variants")," in ReScript? A cousin of regular variants, with some drawbacks and some advantages compared to regular variants. One of the advantages is that polyvariants can be ",(0,r.kt)("em",{parentName:"p"},"inferred")," from your code usage. And ResGraph can leverage this inference to produce enums automatically for your GraphQL schema."),(0,r.kt)("p",null,"Let's look at an example:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-rescript"},"let displayName = (user: user, ~format) => {\n  switch format {\n    | Some(#Long) => `${user.firstName} ${user.lastName}`\n    | Some(#Short) => user.firstName\n    | Some(#Initials) => getInitials(user)\n    | None => `${user.firstName} ${user.lastName}`\n  }\n}\n")),(0,r.kt)("p",null,"This produces this schema:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-graphql"},"enum UserDisplayNameFormat {\n  Long\n  Short\n  Initials\n}\n\ntype User {\n  displayName(format: UserDisplayNameFormat): String!\n}\n")),(0,r.kt)("p",null,"Notice there's no type annotation for the ",(0,r.kt)("inlineCode",{parentName:"p"},"format")," argument. It's all inferred by ReScript from your usage in the code. And from that, ResGraph can infer a enum for you. This works in the following places:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"As arguments"),(0,r.kt)("li",{parentName:"ul"},"As return types"),(0,r.kt)("li",{parentName:"ul"},"Nested inside of other inferred objects (regular and input objects)")),(0,r.kt)("p",null,'This is really neat for rapidly prototyping things, and there certainly are "build-for-a-single-use" scenarios where this makes sense. Each inferred enum will be automatically named according to its context. For arguments, it\'ll be named ',(0,r.kt)("inlineCode",{parentName:"p"},"<parentTypeName><fieldName><argumentName>"),". For return types ",(0,r.kt)("inlineCode",{parentName:"p"},"<parentTypeName><fieldName>"),", and so on."),(0,r.kt)("p",null,"You're advised to ",(0,r.kt)("em",{parentName:"p"},"use this to work quickly"),", but then solidify your enum into an actual declared ",(0,r.kt)("inlineCode",{parentName:"p"},"@gql.enum")," when you're done working, unless you really don't need the benefits of a \"real\" enum. Here are the downsides of using an inferred enum compared to an actual declared one:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Can't use doc strings"),(0,r.kt)("li",{parentName:"ul"},"Can't control runtime representation with ",(0,r.kt)("inlineCode",{parentName:"li"},"@as")),(0,r.kt)("li",{parentName:"ul"},"Can't control the name of the generated type"),(0,r.kt)("li",{parentName:"ul"},"Can't reuse the type unless you declare it (at which point you might as well just make a regular ",(0,r.kt)("inlineCode",{parentName:"li"},"@gql.enum"),")")),(0,r.kt)("p",null,"Still, inferred enums are really cool! Use them to work quickly without type declarations needing to get into your way."),(0,r.kt)("h2",{id:"next-steps"},"Next steps"),(0,r.kt)("p",null,"Let's look at how enum's close friend ",(0,r.kt)("a",{parentName:"p",href:"unions"},"unions are defined")," in ResGraph."))}c.isMDXComponent=!0}}]);