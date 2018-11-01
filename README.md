# ursus-maritimus

Haskell port of the <a href="https://github.com/YuvalItzchakov/ursus-martimus">scala ursus-martimus</a> repo

Ursus maritimus (A.K.A Polar Bear) is a tool for processing synthetically generated data in a streaming fashion.

### Architecture:

```markdown
+------------------------------------------------------------------------+
|                                                                        |
|                                                                        |
|   Data Generation Source             HTTP Endpoint                     |
|             +                              +                           |
|             |                              |                           |
|             |                              |                           |
|             v                              |                           |
|        JSON parser                         |  Query aggregated events  |
|             +                              |                           |
|             |                              |                           |
|             |                              |                           |
|             v         Publish to           v                           |
|       Raw event store +--------> Accumulated event store               |
|                                                                        |
|                                                                        |
+------------------------------------------------------------------------+
```

The architecture resembles the CQRS architecture where the write database
is an append only log. After committing to the log, events are published to the
read database which saves data in a more efficient manner for querying the data
via the HTTP API.

Events in the accumulated event store are *eventually consistent*, in the sense that first
events are written to the write store, they will be propogated to the read store and only then be 
visible to the user under the HTTP endpoint.

### The following are prerequisites to running the application:

1. GHC 8.4.3
2. Stack
3. Data generation tool which generates events with the following JSON schema:

```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "event_type": {
      "type": "string"
    },
    "data": {
      "type": "string"
    },
    "timestamp": {
      "type": "integer"
    }
  },
  "required": [
    "event_type",
    "data",
    "timestamp"
  ]
}
```

### How to run the application:

- stack build  
- stack exec ursus-maritimus-exe [path to data generator]
    
### Querying the data API

After running the service, the process starts an HTTP service which by default binds 
to "0.0.0.0:8080". If you'd like to change the port, this can be configured via 
the **application.conf**, and a restart to the service.

#### Endpoints exposed:

1. Event count by type - Represents events received so far, grouped by event type:
    - URL: [IP:Port]/eventsbytype
    - Verb: GET
    
2. Event count by data - Represents events received so far, grouped by event data (the payload supplied by the event)
    - URL: [IP:Port]/eventsbydata
    - Verb: GET