# Ada_Sqlite3 Tests

This directory contains unit and integration tests for the Ada_Sqlite3 library using AUnit.

## Test Categories

### Database Tests
- Database lifecycle management (open, close)
- Basic SQL operations (execute, query)
- Transaction handling (begin, commit, rollback)
- Error conditions (invalid paths, SQL syntax)
- Version information
- Resource cleanup and finalization
- Memory handling

### Statement Tests
- Prepared statement lifecycle
- Parameter binding:
  * Indexed parameters (?, ?, ?)
  * Named parameters (:name, :value)
  * Different data types (NULL, INTEGER, REAL, TEXT)
- Result processing:
  * Column access and type checking
  * Row iteration
  * Result set metadata
- Statement cleanup and finalization
- Error handling and recovery

### BLOB Tests
- BLOB creation and manipulation
- Stream interface operations:
  * Reading data
  * Writing data
  * Position tracking
- Size handling:
  * Empty BLOBs
  * Small BLOBs (< 1KB)
  * Large BLOBs (100KB+)
- Database storage and retrieval
- Memory management
- Resource cleanup

### Generic Function Tests
- Scalar functions:
  * Function registration
  * Argument handling
  * Result type management
  * Context management
- Aggregate functions:
  * Step processing
  * State accumulation
  * Final value computation
- Window functions:
  * Frame management
  * State tracking
  * Inverse operations
- UTF-16 text support
- Error conditions:
  * Bounds checking
  * Empty arguments
  * Type mismatches
- Multiple function registration
- Context cleanup

## Test Infrastructure

### AUnit Framework
The test suite uses the AUnit testing framework, providing:
- Test case organization
- Assertion handling
- Test suite aggregation
- Result reporting

### Test Runner Configuration
The test runner (tests/harness/test_runner.adb) provides:
- Command-line options
- Test filtering
- Detailed reporting
- Status codes

## Test Coverage

### Memory Management
- Resource allocation tracking
- Cleanup verification
- Handle management
- Memory leak detection

### Error Handling
- Invalid operations
- Resource exhaustion
- Type mismatches
- Bounds checking
- SQL errors

### Resource Management
- Statement finalization order
- Database connection cleanup
- BLOB resource tracking
- Context cleanup for custom functions

### Edge Cases
- Empty results
- Large datasets
- Unicode text handling
- Transaction nesting
- Concurrent operations

## Building and Running Tests

### Build Instructions
```bash
# Navigate to the tests directory
cd tests

# Build the test suite
gprbuild -P ada_sqlite3_tests.gpr

# Run the tests
./bin/test_runner
```