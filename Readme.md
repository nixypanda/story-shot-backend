Backend
--

The back-end for story-shot app written in Haskell.

Steps (You will need the [haskell-stack](https://docs.haskellstack.org/en/stable/README) on your
machine):
1. `cd` into the directory (backend).
2. run `stack setup`.
3. run `stack build`.
4. run `stack exec story-shot-exe`.


## Structure
- *Type*: This layer defines
  - specific type used in the application
  - helpers around that type
  - Json conversion for the type
  - Opelaye related stuff to set up the specific type
- *Storage*: This layer defines
  - representation of db rows as is into some Haskell type.
- *Resource*: This layer takes care of
  - linking up different resources
  - foreign keys or associations resolution
- *Controller*: This layer takes care of
  - JSON marshalling and unmarshalling
  - handle exceptions resulting from json marshalling
  - Make simplified JSON-api responses


## API Docs
| Endpoint | Method | Description |
|----------|--------|-------------|
| `/` | `GET` |  index route |
| /health | `GET`| health |
| | | |
| `/story`        | `POST`   |  |
| `/story`        | `GET`    | Batch request, supported query params: includes={author,tag}, next_cursor={Int}, size={Int} |
| `/story/:id`    | `GET`    | Single, supported query params: includes={author,tag} |
| `/story/random` | `GET`    | get a random story, supported query params: includes={author,tag}|
| `/story`        | `PUT`    | edit multiple stories |
| `/story/:id`    | `PUT`    | edit a specific story |
| `/story`        | `DELETE` | delete multiple stories Batch, supported query params: ids={Comma seperated Int} |
| `/story/:id`    | `DELETE` | delete |
| | | |
| `/tag`     | `POST`   | |
| `/tag`     | `GET`    | Batch request, supported query params: next_cursor={Int}, size={Int} |
| `/tag/:id` | `GET`    | |
| `/tag`     | `PUT`    | |
| `/tag/:id` | `PUT`    | |
| `/tag`     | `DELETE` | delete multiple tags Batch, supported query params: ids={Comma seperated Int} |
| `/tag/:id` | `DELETE` | |
| | | |
| `/author`     | `POST`   | |
| `/author`     | `GET`    | Batch request, supported query params: next_cursor={Int}, size={Int} |
| `/author/:id` | `GET`    | |
| `/author`     | `PUT`    | |
| `/author/:id` | `PUT`    | |
| `/author`     | `DELETE` |delete multiple authors Batch, supported query params: ids={Comma seperated Int} |
| `/author/:id` | `DELETE` | |
| | | |
| `/user`     | `POST`   | |
| `/user`     | `GET`    | Batch request, supported query params: includes={author}, next_cursor={Int}, size={Int} |
| `/user/:id` | `GET`    | |
| `/user`     | `PUT`    | |
| `/user/:id` | `PUT`    | |
| `/user`     | `DELETE` | |
| `/user/:id` | `DELETE` | delete multiple users Batch, supported query params: ids={Comma seperated Int} |


### Todo
- [x] Basic Application Setup
- [x] Basic controller setup
- [x] Author: CRUD
- [x] Tag: CRUD
- [x] Story: CRUD
- [x] Simplified JSON-API response
- [x] Detailed response on invalid data
- [x] Error/Exception Handling
- [ ] Transaction Blocks
- [x] DB Connection Pooling
- [x] Pagination
- [x] Abstract Metadata info for all resources
- [x] Session Creation
- [ ] Session Expiry
- [ ] Microservice: Tag analyser (Uses NLP to figure out tags for a story)
- [ ] Microservice: Story Recommender (Recommends stories based on previous reads)
