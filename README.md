# Damn Fast Priority Queue

A heap-based priority queue whose first and foremost priority is [**speed**](https://www.youtube.com/watch?v=AkagvXwDsYU).

Blame [@mfiano](https://github.com/mfiano/) for the existence of this library. He's the one who wanted a priority queue that's going to run as fast as possible in one of the hottest loops of his game engine ~~and then figured out that hey, he actually doesn't need a prio queue there~~.

## Description

* The queue enqueues objects along with their priorities.
  * The stored objects may be of arbitrary type.
  * The objects' priorities must be of type `(unsigned-byte 32)`.
* The queue is a minimum queue (i.e. smallest priorities are dequeued first).
* The queue is unbounded.
  * The queue's storage automatically expands (which reallocates the queue storage).
  * The queue's storage can be manually trimmed (which reallocates the queue storage).
* The queue does not preserve FIFO order for elements with the same priority.
* The queue is **not** thread-safe.
* The queue is **not** reentrant.

## Implementation details

* The queue internally uses two simple vectors: one for data, specialized on `t`, and another for priorities, specialized on `(unsigned-byte 32)`.
* The queue's storage has its initial size set to the extension size.
* Each time the queue runs out of storage, the storage is reallocated via `adjust-array` and its size is expanded by the `extension-size` value provided at queue instantiation.
* We assume that using simple vectors, calling `adjust-array` on them, and manually setting queue slots to the new vectors is faster than using adjustable vectors.

## Optimization settings

* The code uses structure classes in favor of standard classes.
* The code uses standard, `inline`-proclaimed functions in favor of generic functions.
* All functions are optimized for maximum `speed`.
* By default, the code retains the default values of `debug`, `safety`, `space`, and `compilation-speed` optimize qualities. To set them all to 0, pray to your favorite deity and push `:real-damn-fast-priority-queue` into `*features*` before compiling the system.

## Exports

All exported functions are proclaimed `inline` by default.

* **Classes**
  * `queue` - names the priority queue structure class.
* **Functions**
  * `(make-queue &optional extension-size)` - make a priority queue with a given extension size.
    * The extension size value must be a positive integer.
    * The default extension size is `256`. 
  * `(enqueue queue object priority)` - enqueue an object.
  * `(dequeue queue)` - dequeue an object.
    * Secondary return value is true if the object was found and false if the queue was empty.
  * `(peek queue)` - peek at an object that is first to be dequeued.
    * Secondary return value is true if the object was found and false if the queue was empty.
  * `(size queue)` - get the current element count of the queue.
  * `(trim queue)` - trim the queue's storage by calling `adjust-array` on it with the current queue size.

## Tests

* Non-verbose test: `(asdf:test-system :damn-fast-priority-queue)` or `(asdf:load-system :damn-fast-priority-queue/test) (damn-fast-priority-queue/test:run)`
* Verbose test: `(asdf:load-system :damn-fast-priority-queue/test) (damn-fast-priority-queue/test:run t)`

## License

MIT.
