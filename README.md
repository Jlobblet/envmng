# envmng
envmng is a tool for managing shell environments.
I made it because I was fed up by the slow start-up time my shell was exhibiting.

## Installation

`stack install`

Add the following snippet to your `.zshrc`, `.bashrc`, or similar:

```zsh
envmng() {
	eval $(envmng-exe "$@")
}
```

## Usage

```
envmng +d
envmng +ruby
envmng -java

envmng-exe --help
envmng-exe list
```

