# jsn

**jsn** (pronunciation: json) - an absolutely simple & terse CLI app to manipulate JSON data. You either learn to use it in less than 10 minutes, or I failed.

Goals:

1. As terse, simple and mnemonic as possible, so that you can learn the core functionality in 10 minutes.
1. Once you learn it, you can instantly use it efficiently - out-of-box experience thanks to the sane defaults.
1. The less features, the better. Unless requested features are proven to have a common application.
1. Good at one thing - JSON data manipulation. No stuff like mathematical operations, as there are better tools for it. Prioritize performance, perfect error handling and usability with other apps over new features.

So, what is it actually good at?

- doing quick modifications to JSON files from command line - it's great for configuration files like `package.json`, where all you want to do is insert/remove/change a small portion of an already big and complexed (in terms of structure) file
- general operations on JSON data, thanks to its streaming interface - you can combine (pipe) it with other apps for unlimited possibilites - it's quite fast too

## Start

[Download](https://github.com/goldenlynx/jsn/releases): there are binaries available for macOS and Ubuntu. I did not test any other platform yet.

To get started, complete [this short tutorial](#learn-it-in-10-minutes). Run `jsn -h` for additional help.

## Learn it in 10 minutes

Firstly, let's define 2 terms which are used across this project to keep things simple:

- **member** - an element of JSON array or a value of JSON object
- **key** - an index of JSON array or a key of JSON object

A list of keys forms a path to access a member.

jsn has commands, options and flags. Each of them is always denoted by one letter.

**Every command** consumes a **JSON input** to perform some manipulations on it and produces a **JSON output**.

By default, input is read from stdin and output is written to stdout. You can also use these options to change it:

- `-f FILE` to read input from a **f**ile named `FILE`
- `-i FILE` to perform an **i**n-place edit - read input from `FILE` and write output to it

Ok, let's run our first command. Consider a file called `file.json`:

```json
{
  "data": {
    "numbers": [0, 1, 2]
  }
}
```

We modify it by **i**nserting (`i` command) a new number into the list:

```sh
jsn i '.data.numbers' '3' -i file.json
```

`file.json` is now:

```json
{
  "data": {
    "numbers": [0, 1, 2, 3]
  }
}
```

How about inserting an element to the beginning of the array? Well, we need to use a **reversed variant** of our **i**nsert command. That is right: some jsn commands have a **reversed variant**. It is denoted by an uppercase command name:

```sh
jsn I '.data.numbers' 'null' -i file.json
```

`file.json` is now:

```json
{
  "data": {
    "numbers": [null, 0, 1, 2, 3]
  }
}
```

The syntax for almost every jsn command is more or less the same:

```sh
jsn COMMAND PATH [VALUE]
```

Where:

- `COMMAND` is a command name (uppercase for a reversed variant)
- `PATH` is a path pattern to find **members**. It's syntax is: `.key1.key2.key3`, but generally `key1.key2.key3` is fine too. An empty string (`''`) means a root member, so just the JSON input itself. Keep in mind that 0, 1 or more paths can match a path pattern. For example, our previous file has 1 member for pattern `.data.numbers`, but 0 members for pattern `.numbers`. To match multiple paths, we can use wildcards, which are similar to **glob**: `*` means **exactly one key** and `**` means **0 or more keys**. So, given our example, we could also use path pattern `.**.numbers` or `.*.*`. Both of them would perform the same insertion.
- `VALUE`, which exists only for some commands (like insert), is just a JSON value. The cool part is that, once it's missing, it will be also read from stdin. This provides us a great interface for piping jsn commands!

---

Let's consider a new file, `primes.json`:

```json
[2, 3, 5, 7, 11]
```

We want to **t**ake the last 3 primes and **c**oncatenate them with our previous array of numbers. Alright, let's do it:

```sh
jsn t '' 3 -f primes.json | jsn c 'data.numbers' -i file.json
```

`file.json` is now:

```json
{
  "data": {
    "numbers": [null, 0, 1, 2, 3, 5, 7, 11]
  }
}
```

How does it work?

1. Command `t` (syntax: `t PATH N`) reads input from **f**ile `primes.json` and **t**akes last 3 (`N` argument) elements from it - a path pattern `''` means the root of JSON input.
1. Command `c` (syntax: `c PATH VALUE`), thanks to the pipe operator, uses the previous output for a `VALUE` argument and **c**oncatenates it with a member at `data.numbers` path. The input is read from `file.json` and written to the same file (`-i` for an **i**n-place edit).

---

For our last example, consider this input:

```json
{
  "array": [
    {
      "list": [
        null,
        {
          "foo": true
        }
      ]
    }
  ]
}
```

How do we set the `true` value to `false`? We need a **s**et command (`s`):

```sh
jsn s '.array.0.list.1.foo' 'false'
```

It works, but it's not great. How can we access the value in a more elegant way? Wildcards!

```sh
jsn s '.*.*.*.*.foo' 'false'
```

Or even just:

```sh
jsn s '.*.*.*.*.*' 'false'
```

Or...

```sh
jsn s '.**.foo' 'false'
```

Much more elegant, right?

### Commands cheatsheet

| name | arguments            | action                                                        | reversed variant
|------|----------------------|---------------------------------------------------------------|-----------------------------------
| `g`  | `PATH [-a]`          | **g**et all values matching path (`-a` - **a**rray of values) | -
| `s`  | `PATH VALUE`         | **s**et member value                                          | -
| `n`  | `PATH VALUE`         | **n**ew member (like `s`, but can create a new member)        | -
| `d`  | `PATH`               | **d**elete member                                             | -
| `k`  | `PATH`               | **k**eys of object                                            | -
| `v`  | `PATH`               | **v**alues of object                                          | -
| `l`  | `PATH`               | **l**ength of array or object                                 | -
| `r`  | `PATH`               | **r**everse array                                             | -
| `t`  | `PATH N [-b IND=0]`  | **t**ake N elements from array, **b**egin at IND (default: 0) | take elements from the beginning
| `p`  | `PATH [N=1]`         | **p**op N (default: 1) elements from array                    | pop elements from the beginning
| `i`  | `PATH VALUE`         | **i**nsert `VALUE` to array                                   | insert element to the beginning
| `c`  | `PATH VALUE`         | **c**oncatenate array and `VALUE` (`VALUE` must be array)     | concatenate `VALUE` and array
| `m`  | `PATH VALUE`         | **m**erge object and `VALUE` (`VALUE` must be object)         | overwrite existing keys
| `f`  | `PATH [KEYS...]`     | **f**ilter (keep) members of a member by keys                 | filter out (discard) members

### Gotchas

#### No path matched our path pattern

If one or more members matches `PATH` pattern, the command action will be performed on every matched member - starting from the deepest ones. If `PATH` doesn't match anything, you will get an error (a sane default, right?). How to disable it? Use `-z` flag which stands for: **z**ero paths matched allowed. So, what you will get with a `-z` flag enabled and 0 paths matched? Naturally, output will be the same as input.

#### Tolerant mode

It can happen, that some paths matched don't make sense for your command and generate an error. For example, concatenation command `jsn c '*' '[1, 2, 3]'` will fail for this input:

```json
{
  "a": [0],
  "b": true
}
```

While member at path `.a` is an array, member at path `.b` clearly isn't. What if we would like to ignore those fails? We can use a **t**olerant mode with flag `-t`. A command `jsn c '*' '[1, 2, 3]' -t` will give us our desired result:

```json
{
  "a": [0, 1, 2, 3],
  "b": true
}
```

#### Get command (`g`)

Every command shares the same output format. The only exception is a **g**et command (`g`). If the `PATH` pattern matches zero or one path, then it's quite the same. What happens if it matches many members, though? The answer is: it will write multiple outputs. While it **can** be useful to use with other apps, it's impossible to pipe it through jsn itself - jsn always expects one input. That's why you can use an `-a` flag, which stands for "**a**rray output". All the outputs will be combined to one array of JSON values - which is a valid JSON value. Voila!

#### New command (`n`)

`n` command is quite unique. It's behaviour is simple: do what set command (`s`) does, but accept a case where there is no such a member yet.

Consider an input:

```json
{
  "list": [1, 1, 2, 3]
}
```

So, while both `jsn s '.list.0' '0'` and `jsn n '.list.0' '0'` will produce:

```json
{
  "list": [0, 1, 2, 3]
}
```

A command `jsn s '.list.4' '4'` will simply fail - there is no such a member. Command `jsn n '.list.4' '4'` will do just fine, though:

```json
{
  "list": [0, 1, 2, 3, 4]
}
```

#### Filter command (`f`)

`f` command accepts `KEYS` argument to filter... members of a member. This doesn't sound like a straightforward thing, even though it really is. Suppose we have this input:

```json
{
  "data": {
    "a": 1,
    "b": 2,
    "c": 3
  }
}
```

If we run `jsn f '.data' 'a,b'` we get in return:

```json
{
  "data": {
    "a": 1,
    "b": 2
  }
}
```

And the reversed variant: `jsn F '.data' 'a,b'`? Couldn't be any more obvious:

```json
{
  "data": {
    "c": 3
  }
}
```

You can either separate keys by a comma, or just use multiple arguments. Heck, you can even do both: `jsn F '.data' 'key1,key2' 'key3' 'key4'`!

#### "I only want the modified part"

Every jsn command will always return the modified root member of input as output. Such a consistency is great, but very often it can be counter intuitive. Consider such a file:

```json
{
  "numbers": [1, 2, 3],
  "otherData": "foobar"
}
```

What's the output of the command: `jsn t '.numbers' 2`? It is:

```json
{
  "numbers": [2, 3],
  "otherData": "foobar"
}
```

"What?! But I've literally asked to **take 2 elements**, nothing else!".

I see your point! To use jsn commands like this, just apply `-m` flag, which stands for a "**m**odified part". This way, a command `jsn t 'numbers' 2 -m` would give us:

```json
[2, 3]
```

It even works with wildcards matching multiple paths. Although, you need to be aware that paths are always matched starting from the deepest members and in such an order all the operations are performed. So what you eventually get is the outermost modified member.

#### Those boring edge cases

Sometimes, out of insanity, people will be tempted to use JSON standard to the highest extent and use the following characters for their keys:

- empty string (`''`)
- dots (`.`)
- stars (`*`)

While it's far from being a popular practise, **jsn does support it**. To not collide with other features, you simply need to apply an escape operator (`\`). So, to access the deepest member in input like:

```json
"**": {
  "": {
    ".\\": true
  }
}
```

One has to create such a path:

```none
.\*\*..\.\\
```

Which is not the most readable line of code ever, but people used to regular expression will surely not mind :) .

---

And that's the entire jsn API. I hope that reading this didn't take you much more than the promised 10 minutes!

### Pretty print

By default, every output will be pretty printed. If you want to change the **w**hitespace characters used for indentation (default: 2 spaces), use `-w SPACES` option, where `SPACES` is the amount of spaces. If you want tabs, simply use `'t'` value.

If you want to disable pretty printing, just use the `-u` (**u**gly) flag.

## Why is it so terse?

And why is vim so terse with its commands? Because **small tasks require small amount of time spent on them**. Way too often I need to perform a quick JSON modification, which turns out to be **not so quick** at all.

I struggle. No trailing commas allowed - oh, what a shame. I can not just omit double quotes like in JavaScript? Not so great. Oh, I need to copy and paste values here and there, because there is no simpler way for it. **My frustration has led me to creating jsn**.

If you really dislike this convention, simply `alias` everything.

## Contributions

Are welcome in any form. Share your bugs, ideas, pull requests. Add tests or refactor the code. Thank you very much for your attention, you're the best ;) .

## Alternatives

Almost every other tool built for JSON manipulations does so much more. jsn was built to be as dumb as it's possible, so that you can pick it up quickly. However, the following tools are great for more advanced things:

- [jq](https://stedolan.github.io/jq/) - it has pretty much **everything** you could think of
- [underscore-cli](https://github.com/ddopson/underscore-cli) - a very powerful JS-like interface

## TODO

Add tests. Add benchmarks to measure and improve perf.

## License

MIT
