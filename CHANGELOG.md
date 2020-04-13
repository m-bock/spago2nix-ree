# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.0.0] - 2020-04-13

### Added

- Support for local packages.

### Changed

- Format of lock file.
- Much better caching of already built packages. Version `1.0.0` was practically unusable for more than a couple of dependencies.
- The CLI works now on `packages.dhall` instead of `spago.dhall`. This makes much more sense.

## [1.0.0]

### Added

- Initial implementation
