"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[664],{3905:(e,t,r)=>{r.d(t,{Zo:()=>l,kt:()=>f});var n=r(7294);function o(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function a(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function i(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?a(Object(r),!0).forEach((function(t){o(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):a(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function s(e,t){if(null==e)return{};var r,n,o=function(e,t){if(null==e)return{};var r,n,o={},a=Object.keys(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||(o[r]=e[r]);return o}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(o[r]=e[r])}return o}var c=n.createContext({}),p=function(e){var t=n.useContext(c),r=t;return e&&(r="function"==typeof e?e(t):i(i({},t),e)),r},l=function(e){var t=p(e.components);return n.createElement(c.Provider,{value:t},e.children)},u="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},m=n.forwardRef((function(e,t){var r=e.components,o=e.mdxType,a=e.originalType,c=e.parentName,l=s(e,["components","mdxType","originalType","parentName"]),u=p(r),m=o,f=u["".concat(c,".").concat(m)]||u[m]||d[m]||a;return r?n.createElement(f,i(i({ref:t},l),{},{components:r})):n.createElement(f,i({ref:t},l))}));function f(e,t){var r=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var a=r.length,i=new Array(a);i[0]=m;var s={};for(var c in t)hasOwnProperty.call(t,c)&&(s[c]=t[c]);s.originalType=e,s[u]="string"==typeof e?e:o,i[1]=s;for(var p=2;p<a;p++)i[p]=r[p];return n.createElement.apply(null,i)}return n.createElement.apply(null,r)}m.displayName="MDXCreateElement"},543:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>c,contentTitle:()=>i,default:()=>d,frontMatter:()=>a,metadata:()=>s,toc:()=>p});var n=r(7462),o=(r(7294),r(3905));const a={sidebar_position:9},i="Subscriptions",s={unversionedId:"subscriptions",id:"subscriptions",title:"Subscriptions",description:"You can add a subscription to your schema by defining a subscription type and then attaching resolvers to it. All subscription resolvers must return an AsyncIterator.t with a valid GraphQL type.",source:"@site/docs/subscriptions.md",sourceDirName:".",slug:"/subscriptions",permalink:"/resgraph/docs/subscriptions",draft:!1,editUrl:"https://github.com/zth/resgraph/tree/main/docs/templates/shared/docs/subscriptions.md",tags:[],version:"current",sidebarPosition:9,frontMatter:{sidebar_position:9},sidebar:"tutorialSidebar",previous:{title:"Mutations",permalink:"/resgraph/docs/mutation"},next:{title:"Best Practices",permalink:"/resgraph/docs/best-practices"}},c={},p=[],l={toc:p},u="wrapper";function d(e){let{components:t,...r}=e;return(0,o.kt)(u,(0,n.Z)({},l,r,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"subscriptions"},"Subscriptions"),(0,o.kt)("p",null,"You can add a subscription to your schema by defining a ",(0,o.kt)("inlineCode",{parentName:"p"},"subscription")," type and then attaching resolvers to it. All subscription resolvers ",(0,o.kt)("em",{parentName:"p"},"must return an ",(0,o.kt)("a",{parentName:"em",href:"https://rescript-lang.org/docs/manual/latest/api/core/asynciterator"},(0,o.kt)("inlineCode",{parentName:"a"},"AsyncIterator.t")))," with a valid GraphQL type."),(0,o.kt)("p",null,"Let's look at an example:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-rescript"},"// Define the subscription type\n@gql.type\ntype subscription\n\nlet wait = ms => {\n  Promise.make((resolve, _) => {\n    let _ = setTimeout(() => resolve(), ms)\n  })\n}\n\n@gql.field\nlet countdown = (_: subscription, ~from: int) => {\n  let countdown = ref(from)\n  let iterator = AsyncIterator.make(async () => {\n    await wait(500)\n    let current = countdown.contents\n    countdown := current - 1\n\n    if current > 0 {\n      AsyncIterator.value(current)\n    } else {\n      AsyncIterator.done(~finalValue=current)\n    }\n  })\n\n  iterator\n}\n\n")),(0,o.kt)("p",null,"This would produce the following schema:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-graphql"},"type Subscription {\n  countdown(from: Int!): Int!\n}\n")))}d.isMDXComponent=!0}}]);