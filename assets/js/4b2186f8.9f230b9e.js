"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[232],{3905:(e,t,n)=>{n.d(t,{Zo:()=>h,kt:()=>g});var r=n(7294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function s(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?s(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):s(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},s=Object.keys(e);for(r=0;r<s.length;r++)n=s[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var s=Object.getOwnPropertySymbols(e);for(r=0;r<s.length;r++)n=s[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var p=r.createContext({}),c=function(e){var t=r.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},h=function(e){var t=c(e.components);return r.createElement(p.Provider,{value:t},e.children)},l="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,s=e.originalType,p=e.parentName,h=o(e,["components","mdxType","originalType","parentName"]),l=c(n),m=a,g=l["".concat(p,".").concat(m)]||l[m]||u[m]||s;return n?r.createElement(g,i(i({ref:t},h),{},{components:n})):r.createElement(g,i({ref:t},h))}));function g(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var s=n.length,i=new Array(s);i[0]=m;var o={};for(var p in t)hasOwnProperty.call(t,p)&&(o[p]=t[p]);o.originalType=e,o[l]="string"==typeof e?e:a,i[1]=o;for(var c=2;c<s;c++)i[c]=n[c];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}m.displayName="MDXCreateElement"},9183:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>p,contentTitle:()=>i,default:()=>u,frontMatter:()=>s,metadata:()=>o,toc:()=>c});var r=n(7462),a=(n(7294),n(3905));const s={sidebar_position:100},i="Integrating with existing GraphQL schemas",o={unversionedId:"integrating-with-existing-graphql-schemas",id:"integrating-with-existing-graphql-schemas",title:"Integrating with existing GraphQL schemas",description:"Thanks to ResGraph producing its own GraphQL schema, it's pretty easy to integrate ResGraph into an existing GraphQL JS schema, via schema merging. Let's walk through how to do it.",source:"@site/docs/integrating-with-existing-graphql-schemas.md",sourceDirName:".",slug:"/integrating-with-existing-graphql-schemas",permalink:"/resgraph/docs/integrating-with-existing-graphql-schemas",draft:!1,editUrl:"https://github.com/zth/resgraph/tree/main/docs/templates/shared/docs/integrating-with-existing-graphql-schemas.md",tags:[],version:"current",sidebarPosition:100,frontMatter:{sidebar_position:100},sidebar:"tutorialSidebar",previous:{title:"Subscriptions",permalink:"/resgraph/docs/subscriptions"},next:{title:"Best Practices",permalink:"/resgraph/docs/best-practices"}},p={},c=[{value:"1. Setup ReScript and ResGraph",id:"1-setup-rescript-and-resgraph",level:2},{value:"2. Export the GraphQL schema",id:"2-export-the-graphql-schema",level:2},{value:"3. Merge the ResGraph schema with the existing schema",id:"3-merge-the-resgraph-schema-with-the-existing-schema",level:2},{value:"4. Duplicate the needed types",id:"4-duplicate-the-needed-types",level:2}],h={toc:c},l="wrapper";function u(e){let{components:t,...n}=e;return(0,a.kt)(l,(0,r.Z)({},h,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h1",{id:"integrating-with-existing-graphql-schemas"},"Integrating with existing GraphQL schemas"),(0,a.kt)("p",null,"Thanks to ResGraph producing its own GraphQL schema, it's pretty easy to integrate ResGraph into an existing GraphQL JS schema, via schema merging. Let's walk through how to do it."),(0,a.kt)("blockquote",null,(0,a.kt)("p",{parentName:"blockquote"},"This guide is a work in progress, so there might be inconsistencies.")),(0,a.kt)("h2",{id:"1-setup-rescript-and-resgraph"},"1. Setup ReScript and ResGraph"),(0,a.kt)("p",null,"This does not differ anything from the existing setup instructions, except for one thing - you should replicate your ",(0,a.kt)("inlineCode",{parentName:"p"},"Context")," type from TypeScript into ReScript as ",(0,a.kt)("inlineCode",{parentName:"p"},"ResGraphContext.context"),". Remember that you don't need to write out the entire type if you don't want to, it's fine to just write out the parts you use."),(0,a.kt)("h2",{id:"2-export-the-graphql-schema"},"2. Export the GraphQL schema"),(0,a.kt)("p",null,"The easiest way to export your ResGraph schema to TypeScript is to add a separate file where you link your schema, and then expose that via a ",(0,a.kt)("inlineCode",{parentName:"p"},".d.ts")," file:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-rescript"},"// RescriptGraphQLSchema.res\nlet rescriptGraphQLSchema = ResGraphSchema.schema\n")),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-typescript"},'// RescriptGraphQLSchema.d.ts\nimport { GraphQLSchema } from "graphql";\n\nexport const rescriptGraphQLSchema: GraphQLSchema;\n')),(0,a.kt)("p",null,"There, the ResGraph schema is now exposed to TS."),(0,a.kt)("h2",{id:"3-merge-the-resgraph-schema-with-the-existing-schema"},"3. Merge the ResGraph schema with the existing schema"),(0,a.kt)("blockquote",null,(0,a.kt)("p",{parentName:"blockquote"},"This will assume that you're using ",(0,a.kt)("inlineCode",{parentName:"p"},"graphql-envelop"),".\nMerge your existing schema with the ResGraph schema:")),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-typescript"},'// schema.ts\nimport { mergeSchemas } from "@graphql-tools/schema";\n\nexport const schema = mergeSchemas({\n  schemas: [existingSchema, rescriptGraphQLSchema],\n});\n')),(0,a.kt)("p",null,"You'll also need to set up an ",(0,a.kt)("inlineCode",{parentName:"p"},"Envelop")," plugin. This will make sure that your current schema understands all the ways ResGraph can return GraphQL types."),(0,a.kt)("blockquote",null,(0,a.kt)("p",{parentName:"blockquote"},"Note that this is only something that needs solving when you're using ResGraph with ",(0,a.kt)("em",{parentName:"p"},"something else"),". If all you're using is ResGraph, you don't need to set this up, it'll just work.")),(0,a.kt)("p",null,"For now, the compat ",(0,a.kt)("inlineCode",{parentName:"p"},"Envelop")," plugin doesn't ship with ResGrah. Instead you can copy paste it into your project from below:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-typescript"},'// resgraphCompatPlugin.ts\nimport { Plugin } from "@envelop/core";\nimport { GraphQLSchema } from "graphql";\n\nexport const resgraphCompatPlugin = (): Plugin => {\n  return {\n    onSchemaChange({ schema: s, replaceSchema }) {\n      const schema: GraphQLSchema = s;\n      const unwrapResolverSource = (src: unknown) => {\n        if (typeof src === "object" && src != null) {\n          if ("_0" in src) {\n            return src["_0"];\n          }\n          if ("VAL" in src) {\n            return src["VAL"];\n          }\n        }\n\n        return src;\n      };\n\n      const newSchema = new GraphQLSchema({\n        ...schema.toConfig(),\n        types: Object.values(schema.getTypeMap()).map((type) => {\n          if ("getFields" in type) {\n            const fields = type.getFields();\n            Object.keys(fields).forEach((fieldName) => {\n              const field = fields[fieldName];\n              const defaultResolver = (source: any) => source[fieldName];\n              const originalResolver =\n                "resolve" in field\n                  ? field.resolve ?? defaultResolver\n                  : defaultResolver;\n\n              if ("resolve" in field) {\n                field.resolve = (source, args, context, info) => {\n                  const src = unwrapResolverSource(source);\n                  return originalResolver(src, args, context, info);\n                };\n              }\n            });\n          }\n          return type;\n        }),\n      });\n\n      replaceSchema(newSchema);\n    },\n  };\n};\n')),(0,a.kt)("p",null,"Finally, make sure you add the plugin to your ",(0,a.kt)("inlineCode",{parentName:"p"},"Envelop")," setup:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-typescript"},'import { envelop, useEngine, useSchema } from "@envelop/core";\nimport { resgraphCompatPlugin } from "./resgraphCompatPlugin";\n\nexport const getEnveloped = envelop({\n  plugins: [useSchema(schema), resgraphCompatPlugin(), useEngine(GraphQLJs)],\n});\n')),(0,a.kt)("h2",{id:"4-duplicate-the-needed-types"},"4. Duplicate the needed types"),(0,a.kt)("p",null,"In general, the easiest way for ResGraph and an existing schema to co-exist is to ",(0,a.kt)("em",{parentName:"p"},"duplicate")," types between.\nSo, you can go ahead and add the ",(0,a.kt)("inlineCode",{parentName:"p"},"query"),", ",(0,a.kt)("inlineCode",{parentName:"p"},"mutation")," and ",(0,a.kt)("inlineCode",{parentName:"p"},"subscription")," types to your ResGraph schema if you want to use them with ResGraph."),(0,a.kt)("p",null,"And, any type you want to use from ResGraph that's defined in TypeScript (and vice versa), just go ahead and duplicate the definition of that too. Here's an example for a fictive ",(0,a.kt)("inlineCode",{parentName:"p"},"User")," type:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-typescript"},'// This is the source type that\'s used throughout the GraphQL API from TypeScript\ntype User = {\n  typename: "User";\n  id: string;\n  name: string;\n  age: number | null;\n};\n')),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-rescript"},"// This is the type duplicated to ResGraph so it's usable in ResGraph too\n@gql.type\ntype user = {\n  typename: [#User],\n  id: string,\n  name: string,\n  age: Null.t<int>\n}\n\n")))}u.isMDXComponent=!0}}]);