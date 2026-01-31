# Versioning and changelog policy

masdiff follows semantic versioning (SemVer):

- MAJOR: incompatible API changes.
- MINOR: backwards-compatible feature additions.
- PATCH: backwards-compatible bug fixes.

Changelog discipline:

- All changes land in `CHANGELOG.md` under the **Unreleased** section.
- A release moves Unreleased entries into a dated version header.
- Entries should be grouped by area (e.g., Features, Fixes, Docs, Performance).
- Avoid vague bullets; mention user-visible behavior and APIs affected.
