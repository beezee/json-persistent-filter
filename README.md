# persistent-json-filter

More or less in the title.

If you want to serialize a persistent filter to JSON (say for logging) 
you'll find a ToJSON instance for that.

If you want to deserialize a persistent filter from JSON (say for a generic query API)
you'll find a FromJSON instance for that.

BackendSpecificFilters and UnsafeValues are not support(ed./able?)
