# Writing C Programs

## Compile with `gcc`

- `gcc -Wall infilename.c -o outfilename.o`
	- Basic compile with most compiler warnings enabled.
- `gcc -g -O0 wWall infilename.c -o outfilename.o`
	- Embeds debugging info and disables optimization.

## Debug with `gdb`

- `break linenumber`
	- Creates a breakpoint at the specified line.
- `break file:linenumber`
- `run`
- `c`
	- Continues execution.
- `next`
	- Executes next line.
- `step`
	- Executes next line or steps into function.
- `quit`
- `print expression`
	- Prints the current value of the expression.
- `help command`

## Debug memory problems with `valgrind`

# Our First C Program

## Structure of a `.c` file

```
/* Begin with comments about file contents */

Insert #include statements and preprocessor definitions

Function prototypes and variable declarations

Define main() function
{
	Function body
}

Define other function
{
	Function body
}
```

## Function prototypes

Like variables, functions must be declared before use.

Function prototypes look like **either**:

- `int factorial (int);`
- `int factorial (int n);`

`void` signifies no return value or arguments.

## The `main()` function

Entry point.

Simplest version outputs 0 when successful, and nonzero to signal some error.

- `int main(void);`

Two-argument form allows access to command-line arguments.

- `int main(int argc, char **argv);`

Hello World example:

```c
/* The main() function */
int main(void) /* entry point */
{
	/* write message to console */
	puts("hello world");

	return 0; /* exit (0 => success) */
}
```

Alternative:

```c
int main(void)
{
	const char msg[] = "hello world";

	puts(msg);
}
```

## About strings

Null-terminated (last character in array is `\0` null)
