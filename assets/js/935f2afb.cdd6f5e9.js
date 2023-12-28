"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[53],{1109:e=>{e.exports=JSON.parse('{"pluginId":"default","version":"current","label":"Next","banner":null,"badge":false,"noIndex":false,"className":"docs-version-current","isLast":true,"docsSidebars":{"tutorialSidebar":[{"type":"link","label":"Getting Started","href":"/resgraph/docs/getting-started","docId":"getting-started"},{"type":"link","label":"Object Types","href":"/resgraph/docs/object-types","docId":"object-types"},{"type":"link","label":"Enums","href":"/resgraph/docs/enums","docId":"enums"},{"type":"link","label":"Unions","href":"/resgraph/docs/unions","docId":"unions"},{"type":"link","label":"Interfaces","href":"/resgraph/docs/interfaces","docId":"interfaces"},{"type":"link","label":"Custom Scalars","href":"/resgraph/docs/custom-scalars","docId":"custom-scalars"},{"type":"link","label":"Input Objects","href":"/resgraph/docs/input-objects","docId":"input-objects"},{"type":"link","label":"Query","href":"/resgraph/docs/query","docId":"query"},{"type":"link","label":"Input Unions","href":"/resgraph/docs/input-unions","docId":"input-unions"},{"type":"link","label":"Mutations","href":"/resgraph/docs/mutation","docId":"mutation"},{"type":"link","label":"Subscriptions","href":"/resgraph/docs/subscriptions","docId":"subscriptions"},{"type":"link","label":"Best Practices","href":"/resgraph/docs/best-practices","docId":"best-practices"},{"type":"link","label":"Design Decisions","href":"/resgraph/docs/design-decisions","docId":"design-decisions"},{"type":"link","label":"Developer Experience","href":"/resgraph/docs/developer-experience","docId":"developer-experience"},{"type":"link","label":"GraphQL Yoga","href":"/resgraph/docs/graphql-yoga","docId":"graphql-yoga"},{"type":"link","label":"The Node Interface","href":"/resgraph/docs/node-interface","docId":"node-interface"},{"type":"link","label":"Pagination","href":"/resgraph/docs/pagination","docId":"pagination"},{"type":"link","label":"Relay","href":"/resgraph/docs/relay","docId":"relay"},{"type":"link","label":"Valid GraphQL types in ReScript","href":"/resgraph/docs/valid-graphql-types","docId":"valid-graphql-types"}]},"docs":{"best-practices":{"id":"best-practices","title":"Best Practices","description":"- Annotate resolver functions or inference might surprise you.","sidebar":"tutorialSidebar"},"custom-scalars":{"id":"custom-scalars","title":"Custom Scalars","description":"A custom scalar is a scalar type in your schema where the underlying value is (somewhat) opaque to the client.","sidebar":"tutorialSidebar"},"design-decisions":{"id":"design-decisions","title":"Design Decisions","description":"- Stay as close to ReScript as possible. Where there\'s a ReScript concept or construct to use, use it. Avoid custom things like advanced payloads for attributes as much as possible.","sidebar":"tutorialSidebar"},"developer-experience":{"id":"developer-experience","title":"Developer Experience","description":"DX is a top priority for ResGraph. Therefore, ResGraph comes with its own dedicated tooling.","sidebar":"tutorialSidebar"},"enums":{"id":"enums","title":"Enums","description":"Enums are defined by defining a variant without payloads and annotate it with @gql.enum:","sidebar":"tutorialSidebar"},"getting-started":{"id":"getting-started","title":"Getting Started","description":"Note that ResGraph is currently alpha grade software. Help us out as we work out the details for a stable release.","sidebar":"tutorialSidebar"},"graphql-yoga":{"id":"graphql-yoga","title":"GraphQL Yoga","description":"ResGraph comes with first class support for GraphQL Yoga, a popular and capable GraphQL backend for JavaScript.","sidebar":"tutorialSidebar"},"input-objects":{"id":"input-objects","title":"Input Objects","description":"Input objects are defined by using a record annotated with @gql.inputObject:","sidebar":"tutorialSidebar"},"input-unions":{"id":"input-unions","title":"Input Unions","description":"Even though they\'re not officially in the spec yet, ResGraph has first class support for input unions via the @oneOf server directive proposal.","sidebar":"tutorialSidebar"},"interfaces":{"id":"interfaces","title":"Interfaces","description":"Interfaces are defined by tagging a record with @gql.interface:","sidebar":"tutorialSidebar"},"mutation":{"id":"mutation","title":"Mutations","description":"If your GraphQL API has mutations, you\'ll need to define a mutation type:","sidebar":"tutorialSidebar"},"node-interface":{"id":"node-interface","title":"The Node Interface","description":"You\'re encouraged to read up on the Node interface before reading this text. Here\'s a general article on the Node interface, and here\'s the relevant best practices section from the GraphQL documentation.","sidebar":"tutorialSidebar"},"object-types":{"id":"object-types","title":"Object Types","description":"GraphQL object types are defined using the annotation @gql.type on a ReScript record:","sidebar":"tutorialSidebar"},"pagination":{"id":"pagination","title":"Pagination","description":"You\'re encouraged to read up on connections in GraphQL prior to reading this section. Here\'s an article, and there\'s a link to the official GraphQL documentation on pagination which explains the rationale for using connections well.","sidebar":"tutorialSidebar"},"query":{"id":"query","title":"Query","description":"The heart of your GraphQL server is the Query type. The first thing you\'ll do setting up your GraphQL server is to define your query type:","sidebar":"tutorialSidebar"},"relay":{"id":"relay","title":"Relay","description":"ResGraph has first class support for Relay. Check out the documentation for implementing the Node interface, and how to do pagination using connections.","sidebar":"tutorialSidebar"},"subscriptions":{"id":"subscriptions","title":"Subscriptions","description":"Subscriptions are not yet a thing in ResGraph, but they will be eventually.","sidebar":"tutorialSidebar"},"unions":{"id":"unions","title":"Unions","description":"You define a union by defining a variant with payloads of types tagged with @gql.type, and annotate that variant with @gql.union:","sidebar":"tutorialSidebar"},"valid-graphql-types":{"id":"valid-graphql-types","title":"Valid GraphQL types in ReScript","description":"Given that fields and arguments will be exposed to the outside world through your schema, they can only be of types that can be transformed to valid GraphQL types.","sidebar":"tutorialSidebar"}}}')}}]);