Backend
--

The back-end for story-shot app written in Haskell.

Steps (You will need the [haskell-stack](https://docs.haskellstack.org/en/stable/README) on your
machine):
1. `cd` into the directory (backend).
2. run `stack setup`.
3. run `stack build`.
4. run `stack exec story-shot-exe`.


### Todo
- [x] Basic Application Setup
- [x] Basic controller setup
- [x] Author: CRUD
- [x] Tag: CRUD
- [x] Story: CRUD
- [x] JSON-API response
- [ ] Detailed response on invalid data
- [ ] Error/Exception Handling
- [ ] Transaction Blocks
- [ ] DB Connection Pooling
- [ ] Pagination
