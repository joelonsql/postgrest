# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

PostgREST is a REST API server written in Haskell that automatically generates a RESTful API from any PostgreSQL database schema. It's built for performance using the Warp HTTP server and the Hasql database library with PostgreSQL's binary protocol.

We are working on removing all features from PostgREST except RPC and OpenAPI.

## Development Commands

### Build System
- **Nix (Recommended)**: `nix develop` to enter development shell
- **Stack**: `stack build`, `stack exec postgrest`

### Testing Commands
- `postgrest-test-spec` - Haskell test suite (main tests)
- `postgrest-test-doctests` - Doctest tests  
- `postgrest-test-io` - Python-based IO tests
- `postgrest-test-big-schema` - Large schema tests
- `postgrest-test-replica` - Replica database tests
- `postgrest-coverage` - Full test suite with coverage
- `postgrest-test-memory` - Memory usage tests

To build and test, run this command: `PGDATABASE=postgrest_test PGHOST=localhost PGUSER=joel stack test`

### Code Quality
- `postgrest-style` - Auto-format Haskell, Nix, Python files
- `postgrest-style-check` - Check formatting without changes
- `postgrest-lint` - Lint Haskell, bash scripts, workflows
- `postgrest-check` - Run most CI checks locally

### Development Utilities
- `postgrest-watch <command>` - Watch for changes and re-run command
- `postgrest-git-hooks enable basic|full` - Set up git hooks
- `postgrest-dump-schema` - Export schema cache as JSON

## Architecture Overview

### Core Request Flow
HTTP request → `App.hs` → `ApiRequest/` (parsing) → `Plan/` (query planning) → `Query/` (SQL generation) → PostgreSQL → `Response/` (formatting) → HTTP response

### Key Modules
- **App.hs**: Main application logic, HTTP request to SQL mapping
- **Config.hs**: Configuration management and validation  
- **ApiRequest/**: HTTP request parsing, preferences, query parameters
- **Auth/**: JWT authentication, authorization via PostgreSQL roles
- **Plan/**: Query planning (CallPlan for RPCs, ReadPlan for queries, MutatePlan for mutations)
- **Query/**: SQL generation, query building, prepared statements
- **Response/**: HTTP response formatting, OpenAPI, performance headers
- **SchemaCache/**: Database schema introspection, relationships, identifiers

### Design Principles
- Authorization delegated to PostgreSQL roles and RLS policies
- Stateless design for horizontal scaling
- Direct JSON serialization in SQL for performance
- Schema-driven API with automatic OpenAPI documentation
- Database connection pooling with binary protocol

### Test Structure
- **Haskell Specs** (`test/spec/`): Unit and integration tests using Hspec
- **IO Tests** (`test/io/`): Python-based end-to-end tests using pytest
- **Fixtures**: Database schemas and test data in `test/spec/fixtures/` and `test/io/fixtures.sql`

## Development Workflow

1. Use Nix development environment: `nix develop`
2. Enable git hooks: `postgrest-git-hooks enable basic` 
3. Run comprehensive checks: `postgrest-check`
4. Use watch mode during development: `postgrest-watch postgrest-test-spec`
5. Run tests against single PostgreSQL version for faster feedback
6. Use `postgrest-style` before committing to auto-format code

## Build Configurations

- **Development**: `-O0 -fwrite-ide-info` (fast compilation in cabal.project.local)
- **Production**: `-O2` optimization  
- **Coverage**: `--enable-coverage` with HPC for test coverage reports

## Dependencies

- **Core**: Warp (HTTP server), Hasql (PostgreSQL client), Aeson (JSON)
- **Database**: PostgreSQL 10+ with configurable connection pooling
- **Build**: GHC 9.4.5-9.8.2, Cabal 3.0+, Stack LTS 22.41
- **Tools**: Python 3 for IO tests, Nix for development environment
