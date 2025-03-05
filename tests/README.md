# Ada_Sqlite3 Tests

This directory contains unit and integration tests for the Ada_Sqlite3 library using AUnit.

## Test Structure

The tests are organized into the following categories:

- **Database Tests**: Tests for basic database operations (open, close, execute, etc.)
- **Statement Tests**: Tests for prepared statement operations (prepare, bind, step, etc.)
- **Blob Tests**: Tests for BLOB handling (create, read, write, etc.)
- **Transaction Tests**: Tests for transaction handling (begin, commit, rollback, etc.)

## Building and Running Tests

To build and run the tests:

```bash
# Navigate to the tests directory
cd tests

# Build the tests
gprbuild -P ada_sqlite3_tests.gpr

# Run the tests
./bin/test_runner
```

## Test Coverage

The tests cover the following aspects of the Ada_Sqlite3 library:

### Database Operations
- Opening and closing databases
- Executing SQL statements
- Error handling
- Version information

### Statement Operations
- Preparing and finalizing statements
- Binding parameters (NULL, integer, float, text)
- Stepping through results
- Accessing column data
- Error handling

### BLOB Operations
- Creating and manipulating BLOBs
- Binding BLOBs to statements
- Reading and writing BLOB data
- Handling empty and large BLOBs

### Transaction Operations
- Beginning and committing transactions
- Rolling back transactions
- Nested transactions (savepoints)
- Transaction isolation
- Error handling within transactions

## Adding New Tests

To add new tests:

1. Create a new test package (spec and body) in the `src` directory
2. Add your test procedures to the package
3. Add the package to the test suite in `ada_sqlite3_test_suite.adb`
4. Rebuild and run the tests

## Dependencies

These tests depend on:
- AUnit (Ada Unit Testing Framework)
- Ada_Sqlite3 library
