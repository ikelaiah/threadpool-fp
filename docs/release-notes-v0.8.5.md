# ThreadPool for Free Pascal — v0.8.5

> v0.8.5 is a documentation and project-identity release. Runtime behavior and
> the public API are unchanged from v0.8.0.

## Highlights

- A new banner gives the project a recognizable visual identity. The editable
  [`threadpool-banner.svg`](assets/threadpool-banner.svg) is the source of the
  synchronized 1800×600 PNG shown in the README.
- The README now leads with pool selection and working examples, while detailed
  API, implementation, and troubleshooting material stays in focused documents.
- The new [`CHEATSHEET.md`](CHEATSHEET.md) collects the calls and safety rules
  most users need during implementation.
- Lazarus package metadata now reports version 0.8.5 and uses a shorter package
  description.

## Documentation layout

| Need | Start here |
| --- | --- |
| Choose a pool and run a first task | [README](../README.md) |
| Recall a call or safety rule | [Cheat sheet](CHEATSHEET.md) |
| Explore every public member | [Simple API](ThreadPool.Simple-API.md) or [Producer-Consumer API](ThreadPool.ProducerConsumer-API.md) |
| Understand internals | [Simple technical guide](ThreadPool.Simple-Technical.md) or [Producer-Consumer technical guide](ThreadPool.ProducerConsumer-Technical.md) |
| Upgrade from an earlier lifecycle model | [v0.8.0 release notes](release-notes-v0.8.0.md) |

## Compatibility

No application changes are required when upgrading from v0.8.0. This release
does not change:

- queue ordering or capacity;
- worker-count selection;
- timeout units or meanings;
- shutdown and draining behavior;
- error capture and callback behavior; or
- any public type or method signature.

See the [changelog](../CHANGELOG.md) for the complete version history.
