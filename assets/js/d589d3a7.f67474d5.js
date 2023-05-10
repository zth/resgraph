"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[162],{3905:(e,t,r)=>{r.d(t,{Zo:()=>u,kt:()=>m});var n=r(7294);function a(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function o(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function s(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?o(Object(r),!0).forEach((function(t){a(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):o(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function i(e,t){if(null==e)return{};var r,n,a=function(e,t){if(null==e)return{};var r,n,a={},o=Object.keys(e);for(n=0;n<o.length;n++)r=o[n],t.indexOf(r)>=0||(a[r]=e[r]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)r=o[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(a[r]=e[r])}return a}var l=n.createContext({}),p=function(e){var t=n.useContext(l),r=t;return e&&(r="function"==typeof e?e(t):s(s({},t),e)),r},u=function(e){var t=p(e.components);return n.createElement(l.Provider,{value:t},e.children)},h="mdxType",c={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},d=n.forwardRef((function(e,t){var r=e.components,a=e.mdxType,o=e.originalType,l=e.parentName,u=i(e,["components","mdxType","originalType","parentName"]),h=p(r),d=a,m=h["".concat(l,".").concat(d)]||h[d]||c[d]||o;return r?n.createElement(m,s(s({ref:t},u),{},{components:r})):n.createElement(m,s({ref:t},u))}));function m(e,t){var r=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=r.length,s=new Array(o);s[0]=d;var i={};for(var l in t)hasOwnProperty.call(t,l)&&(i[l]=t[l]);i.originalType=e,i[h]="string"==typeof e?e:a,s[1]=i;for(var p=2;p<o;p++)s[p]=r[p];return n.createElement.apply(null,s)}return n.createElement.apply(null,r)}d.displayName="MDXCreateElement"},9390:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>l,contentTitle:()=>s,default:()=>c,frontMatter:()=>o,metadata:()=>i,toc:()=>p});var n=r(7462),a=(r(7294),r(3905));const o={sidebar_position:1},s="Getting Started",i={unversionedId:"getting-started",id:"getting-started",title:"Getting Started",description:"Note that ResGraph is currently alpha grade software. Help us out as we work out the details for a stable release.",source:"@site/docs/getting-started.md",sourceDirName:".",slug:"/getting-started",permalink:"/resgraph/docs/getting-started",draft:!1,editUrl:"https://github.com/zth/resgraph/tree/main/docs/templates/shared/docs/getting-started.md",tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"tutorialSidebar",next:{title:"Object Types",permalink:"/resgraph/docs/object-types"}},l={},p=[{value:"Setup",id:"setup",level:2},{value:"Installation",id:"installation",level:3},{value:"<code>resgraph.json</code>",id:"resgraphjson",level:3},{value:"<code>ResGraphContext.res</code>",id:"resgraphcontextres",level:3},{value:"Define a query type",id:"define-a-query-type",level:3},{value:"Build and watch mode",id:"build-and-watch-mode",level:4},{value:"Hooking up your schema to GraphQL Yoga",id:"hooking-up-your-schema-to-graphql-yoga",level:3},{value:"How does ResGraph work",id:"how-does-resgraph-work",level:2},{value:"Next steps",id:"next-steps",level:2}],u={toc:p},h="wrapper";function c(e){let{components:t,...r}=e;return(0,a.kt)(h,(0,n.Z)({},u,r,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h1",{id:"getting-started"},"Getting Started"),(0,a.kt)("blockquote",null,(0,a.kt)("p",{parentName:"blockquote"},"Note that ResGraph is currently ",(0,a.kt)("em",{parentName:"p"},"alpha grade software"),". Help us out as we work out the details for a stable release.")),(0,a.kt)("h2",{id:"setup"},"Setup"),(0,a.kt)("p",null,"Let's start by setting up ResGraph in your project."),(0,a.kt)("h3",{id:"installation"},"Installation"),(0,a.kt)("p",null,"ResGraph relies on features in ReScript ",(0,a.kt)("inlineCode",{parentName:"p"},"v11"),", which is currently in alpha. Make sure you run ",(0,a.kt)("inlineCode",{parentName:"p"},">= rescript@11.0.0-alpha.6")," and for the best results, set ",(0,a.kt)("inlineCode",{parentName:"p"},'"uncurried": true')," in your ",(0,a.kt)("inlineCode",{parentName:"p"},"bsconfig.json"),"."),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-bash"},"# Install both `graphql` and `graphql-yoga` so we can set your server up\nnpm i resgraph graphql graphql-yoga @rescript/core @glennsl/rescript-fetch\n")),(0,a.kt)("blockquote",null,(0,a.kt)("p",{parentName:"blockquote"},"We rely on ",(0,a.kt)("inlineCode",{parentName:"p"},"@glennsl/rescript-fetch")," for various fetch related things needed in ",(0,a.kt)("a",{parentName:"p",href:"graphql-yoga"},"GraphQL Yoga"),". We also rely on ",(0,a.kt)("inlineCode",{parentName:"p"},"@rescript/core"),", the (future) standard library for ReScript.")),(0,a.kt)("p",null,"Configure your ",(0,a.kt)("inlineCode",{parentName:"p"},"bsconfig.json"),", including adding the various dependencies to your ",(0,a.kt)("inlineCode",{parentName:"p"},"bs-dependencies"),":"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-json"},'{\n  "uncurried": true,\n  "bs-dependencies": ["resgraph", "@rescript/core", "@glennsl/rescript-fetch"],\n  "bsc-flags": ["-open RescriptCore"],\n  "suffix": ".mjs"\n}\n')),(0,a.kt)("h3",{id:"resgraphjson"},(0,a.kt)("inlineCode",{parentName:"h3"},"resgraph.json")),(0,a.kt)("p",null,"Add a ",(0,a.kt)("inlineCode",{parentName:"p"},"resgraph.json")," file in the root of your ReScript project, and paste this into it:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-json"},'{\n  "src": "./src",\n  "outputFolder": "./src/schema/__generated__"\n}\n')),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},(0,a.kt)("inlineCode",{parentName:"li"},"src")," is the folder where code that can define GraphQL lives"),(0,a.kt)("li",{parentName:"ul"},(0,a.kt)("inlineCode",{parentName:"li"},"outputFolder")," is where you want ResGraph to output the files it generates. ",(0,a.kt)("strong",{parentName:"li"},"Ensure that this folder exists"),". Create it if you don't already have it.")),(0,a.kt)("h3",{id:"resgraphcontextres"},(0,a.kt)("inlineCode",{parentName:"h3"},"ResGraphContext.res")),(0,a.kt)("p",null,"Create a ",(0,a.kt)("inlineCode",{parentName:"p"},"ResGraphContext.res")," file anywhere in your project, and add a ",(0,a.kt)("inlineCode",{parentName:"p"},"context")," type in there. The ",(0,a.kt)("inlineCode",{parentName:"p"},"context")," can have anything you'd like in it:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-rescript"},"// ResGraphContext.res\ntype context = {currentUserId: option<string>}\n")),(0,a.kt)("p",null,"This is the type of your ",(0,a.kt)("a",{parentName:"p",href:"object-types#using-app-context-in-field-functions"},"per-request GraphQL context"),", and ResGraph will ensure you fill out and provide this to every request as you create your server."),(0,a.kt)("h3",{id:"define-a-query-type"},"Define a query type"),(0,a.kt)("p",null,"Somewehere in your project, define a GraphQL ",(0,a.kt)("a",{parentName:"p",href:"query"},(0,a.kt)("inlineCode",{parentName:"a"},"Query"))," type, and a field for it. This is the bare minimum to get a GraphQL server going."),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-rescript"},"// Schema.res - Called `Schema` here, but this can be called anything\n@gql.type\ntype query\n\n/** The current time on the server, as a timestamp. */\n@gql.field\nlet currentTime = (_: query) => {\n  Some(Date.now())\n}\n")),(0,a.kt)("blockquote",null,(0,a.kt)("p",{parentName:"blockquote"},"The root ",(0,a.kt)("inlineCode",{parentName:"p"},"Query")," type is mandatory in a GraphQL schema, and is the base from where all queries will be made.")),(0,a.kt)("p",null,"Make sure ReScript has built your changes (",(0,a.kt)("inlineCode",{parentName:"p"},"npx rescript build"),"). Then run ",(0,a.kt)("inlineCode",{parentName:"p"},"npx resgraph build"),". This should generate your first schema, looking like this:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-graphql"},'type Query {\n  """\n  The current time on the server, as a timestamp.\n  """\n  currentTime: Float\n}\n')),(0,a.kt)("p",null,"ResGraph will automatically generate 3 things by default:"),(0,a.kt)("ol",null,(0,a.kt)("li",{parentName:"ol"},"The ",(0,a.kt)("inlineCode",{parentName:"li"},"ResGraphSchema.res")," and ",(0,a.kt)("inlineCode",{parentName:"li"},"ResGraphSchema.resi")," files. These hold the optimized ",(0,a.kt)("inlineCode",{parentName:"li"},"graphql-js")," schema generated from your types and resolvers, that you then expose through your server."),(0,a.kt)("li",{parentName:"ol"},(0,a.kt)("inlineCode",{parentName:"li"},"ResGraphSchemaAssets.res"),". This holds various generated helpers for your schema. More on that file later."),(0,a.kt)("li",{parentName:"ol"},(0,a.kt)("inlineCode",{parentName:"li"},"schema.graphql"),". This is a schema SDL file dumped from ResGraph. This is intended to be used as a debug utility primarily.")),(0,a.kt)("h4",{id:"build-and-watch-mode"},"Build and watch mode"),(0,a.kt)("p",null,"Notice we built our schema using the one-shot command ",(0,a.kt)("inlineCode",{parentName:"p"},"resgraph build"),". If you want to watch for changes and automatically rerun ResGraph, run ",(0,a.kt)("inlineCode",{parentName:"p"},"resgraph watch"),". However, if you're also using VSCode, you're encouraged to instead use the ",(0,a.kt)("a",{parentName:"p",href:"getting-started"},"ResGraph VSCode extension")," (",(0,a.kt)("strong",{parentName:"p"},"Not yet available, coming very soon"),"), which will run ResGraph in watch mode for you, show you errors and so on, directly inside of VSCode."),(0,a.kt)("p",null,"Excellent, we now have a schema! Let's hook it up to your server."),(0,a.kt)("h3",{id:"hooking-up-your-schema-to-graphql-yoga"},"Hooking up your schema to GraphQL Yoga"),(0,a.kt)("blockquote",null,(0,a.kt)("p",{parentName:"blockquote"},"ResGraph ships with ",(0,a.kt)("a",{parentName:"p",href:"graphql-yoga"},"GraphQL Yoga")," bindings, and we're using it to expose your schema. This is because Yoga is well maintained and well established in the ecosystem. However, hooking up ResGraph to other servers should be simple as well, since the primary artifact of ResGraph is just a ",(0,a.kt)("inlineCode",{parentName:"p"},"graphql-js")," schema.")),(0,a.kt)("p",null,"Let's expose your schema through GraphQL Yoga. Paste this into ",(0,a.kt)("inlineCode",{parentName:"p"},"App.res"),":"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-rescript"},'// App.res\nopen GraphQLYoga\n\nlet yoga = createYoga({\n  schema: ResGraphSchema.schema,\n  context: async ({request}) => {\n    open ResGraphContext\n\n    {\n      currentUserId: request\n        ->Request.headers\n        ->Headers.get("x-user-id"),\n    }\n  },\n})\n\nlet server = NodeHttpServer.createServer(yoga)\n\nlet port = 4555\n\nserver->NodeHttpServer.listen(port, () => {\n  Console.info(`Server is running on http://localhost:${port->Int.toString}/graphql`)\n})\n')),(0,a.kt)("p",null,"After this builds (",(0,a.kt)("inlineCode",{parentName:"p"},"npx rescript build"),", or just run the compiler in watch mode), you can go ahead and run ",(0,a.kt)("inlineCode",{parentName:"p"},"node src/App.mjs")," and you have yourself the simplest possible server running."),(0,a.kt)("p",null,"Before we break down what we did and why, you can ensure everything works by going to ",(0,a.kt)("a",{parentName:"p",href:"http://localhost:4555/graphql"},"GraphiQL")," and trying this query:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-graphql"},"query {\n  currentTime\n}\n")),(0,a.kt)("p",null,"...which should give you back the current timestamp."),(0,a.kt)("p",null,"Now, let's break down what we did and why:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-rescript"},'let yoga = createYoga({\n  schema: ResGraphSchema.schema,\n  context: async ({request}) => {\n    open ResGraphContext\n\n    {\n      currentUserId: request\n        ->Request.headers\n        ->Headers.get("x-user-id"),\n    }\n  },\n})\n')),(0,a.kt)("blockquote",null,(0,a.kt)("p",{parentName:"blockquote"},"Pulling out ",(0,a.kt)("inlineCode",{parentName:"p"},"currentUserId")," is just to examplify that this is per-request context.")),(0,a.kt)("p",null,"This creates your Yoga server by linking together your schema (from the generated file ",(0,a.kt)("inlineCode",{parentName:"p"},"ResGraphSchema"),") to a function that produces your specific app context type."),(0,a.kt)("p",null,"We then create and start the http server exposing our schema:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-rescript"},"let server = NodeHttpServer.createServer(yoga)\n\nlet port = 3000\n\nserver->NodeHttpServer.listen(port, () => {\n  Console.info(`Server is running on http://localhost:${port->Int.toString}/graphql`)\n})\n")),(0,a.kt)("h2",{id:"how-does-resgraph-work"},"How does ResGraph work"),(0,a.kt)("p",null,"At a high level, ResGraph works like this:"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Scans your projects for ",(0,a.kt)("inlineCode",{parentName:"li"},"@gql")," annotations"),(0,a.kt)("li",{parentName:"ul"},"Reads type information, transforms the types and values annotated with ",(0,a.kt)("inlineCode",{parentName:"li"},"@gql")," into a ",(0,a.kt)("inlineCode",{parentName:"li"},"graphql-js")," schema")),(0,a.kt)("p",null,"ResGraph runs on the compiled artifacts ReScript's compiler produces. So, ResGraph needs to run ",(0,a.kt)("em",{parentName:"p"},"after")," the compiler runs. Don't worry though, ResGraph has it's own watcher and build process that's easy to use and handles all of this."),(0,a.kt)("h2",{id:"next-steps"},"Next steps"),(0,a.kt)("p",null,"There! We're all set up and your server is running. We can now continue our ResGraph journey by talking about how to define the primary building block of GraphQL - ",(0,a.kt)("a",{parentName:"p",href:"object-types"},"object types"),"."))}c.isMDXComponent=!0}}]);