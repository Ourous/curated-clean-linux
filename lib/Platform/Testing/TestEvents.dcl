definition module Testing.TestEvents
/**
 * This module provides types representing events occurring during a test run.
 * Each Clean testing framework should report such events on StdOut, as JSON
 * representation of the event types provided here. The test runners
 * (https://gitlab.science.ru.nl/clean-and-itasks/clean-test) process the
 * events further.
 */

from StdOverloaded import class toString
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode, :: Maybe

/**
 * Events that are emitted from tests.
 * Specialised JSONEncode/JSONDecode instances are used for this type, which
 * have to be adapted in case the type definition is changed!
 */
:: TestEvent
	= StartEvent StartEvent //* A test has started
	| EndEvent   EndEvent   //* A test has finished

/**
 * Event emitted when a test is started.
 * Specialised JSONEncode/JSONDecode instances are used for this type, which
 * have to be adapted in case the type definition is changed!
 */
:: StartEvent = { name    :: !String //* The test's name
                }
/**
 * Event emitted after a test has finished.
 */
:: EndEvent   = { name    :: !String       //* The test's name
                , event   :: !EndEventType //* The event's type, indicating success
                , message :: !String       //* Message providing an explanation for the result
                }

/**
 * Indicating the type an end event, indicating whether the test was
 * successful or not.
 * Specialised JSONEncode/JSONDecode instances are used for this type, which
 * have to be adapted in case the type definition is changed!
 */
:: EndEventType = Passed                     //* The test passed
                | Failed !(Maybe FailReason) //* The test failed
                | Skipped                    //* The test was not executed, but should be executed and pass for future versions

/**
 * Reasons for failing a test.
 */
:: FailReason
	= FailedAssertions [FailedAssertion]          //* Assertions that caused the test to fail
	| CounterExamples [CounterExample]            //* Example values for which the test failed
	| FailedChildren [(String, Maybe FailReason)] //* Subtests failed; the tuples are of name and failing reason
	| Crashed                                     //* The test crashed

/**
 * A counter-example to a test.
 */
:: CounterExample =
	{ counterExample   :: ![Expression]      //* The values that disprove the property, in {{`gPrint`}} format
	, failedAssertions :: ![FailedAssertion] //* The assertions that failed in testing the property for that value
	}

:: Expression
	= JSON !JSONNode
	| GPrint !String

/**
 * A failed test assertion.
 * Specialised JSONEncode/JSONDecode instances are used for this type, which
 * have to be adapted in case the type definition is changed!
 */
:: FailedAssertion
	= ExpectedRelation Expression Relation Expression //* A relation test failed

/**
 * A relation between two values.
 * Specialised JSONEncode/JSONDecode instances are used for this type, which
 * have to be adapted in case the type definition is changed!
 */
:: Relation
	= Eq            //* Equality
	| Ne            //* Negated equality
	| Lt            //* Lesser than
	| Le            //* Lesser than or equal to
	| Gt            //* Greater than
	| Ge            //* Greater than or equal to
	| Other !String //* Custom relation

derive JSONEncode TestEvent, StartEvent, EndEvent, FailReason, CounterExample, FailedAssertion, Relation
derive JSONDecode TestEvent, StartEvent, EndEvent, FailReason, CounterExample, FailedAssertion, Relation

instance toString Expression
instance toString Relation
