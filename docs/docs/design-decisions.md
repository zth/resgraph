# Design Decisions

- Stay as close to ReScript as possible. Where there's a ReScript concept or construct to use, use it. Avoid custom things like advanced payloads for attributes as much as possible.
- Focus on speed. Ensure that the DX of using ResGraph is snappy.
- Integrate with the broader ecosystem. Produce a `graphql-js` schema that's easy to use together with the large existing GraphQL ecosystem.
- Fully integrated environment. Dedicated LSP/VSCode extension.
