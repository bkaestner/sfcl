# A simple format for chords and lyrics

## The problem
If you play music, you probably were asked at least once to play
the favourite song of someone you know. You search for
`<title> chords lyrics`, and you end up with something like this:


            Cm                      Ab
    What is up with the chords they don't align anymore
         Eb               Bb
    when I copy them into Word?

                  Cm             Ab
    That's due to non mono-typed font-faces
         Eb               Bb
    they really make this weird.

This is, to be honest, sub-par. Don't get me wrong. It's great to
have at least something, and it's remarkable that the folks have
the dedication to properly place everything in Notepad, vim, or
whatever editor they use.

However, it's often not portable. You have to use a monospaced font,
otherwise you'll end up with misplaced chords. Also, a reader might
eat the additional spaces. This leaves you with something like this:

> Cm                      Ab <br>
> What is up with the chords they don't align anymore <br>
>      Eb               Bb <br>
> when I copy them into Word? <br>
>               Cm             Ab <br>
> That's due to non mono-typed font-faces <br>
>      Eb               Bb <br>
> they really make this weird.

Well, no. That's not going to help anyone, except if you hear the
song over and over again to get the placement right. At that point,
you could already write your own version.

## A solution
The following snippets shows the general idea of the format:

    What is @chord{Cm} up with the chords they @chord{Ab} don't align anymore

    when I @chord{Eb} copy them into @chord{Ab} Word?

This notation is inspired by LaTeX. The chords are defined at the
point you would expect them in the lyrics. Now, that's only slightly
better, after all, we started with something that was aligned just
nicely in the beginning, and now we have to parse the lyrics? Argh!

However, if this followed some kind of grammar, it would be easy to
implement a parser, and pretty print its result to HTML, LaTeX, Word.

## The grammar
*Note: This is a draft.*

The syntax of a song:

    song = line, { line ending , line }
    line = block, { block , [line ending]}

    block   = {spaces} , (clblock | cblock | lblock)
    clblock = cblock , lblock
    cblock  = "@chord{" , chord , "}"
    lblock  = lyrics
    chord   = [^\r\n\}]+
    lyrics  = {[^@\r\n] | "\@" } , lyrics
    spaces  = ? US-ASCII 32 ? | ? US-ASCII 9 ? (* spaces and tabs *)

    line ending = ["\r"] , "\n"

The grammar is given in pseudo-EBNF, except for `chord` and `lyrics`,
which follow the regular expression syntax, e.g. a `chord` may not
contain `\r`, `\n` or `}` and must use at least one character.

This concludes that the following to SFCL snippets would be parsed
the same:

    What is @chord{Cm} up with the chords they @chord{Ab} don't align anymore

    when I  @chord{Eb} copy them into @chord{Ab} Word?

And here's a variant that puts the chords first:

               What is
    @chord{Cm} up with the chords they
    @chord{Ab} don't align anymore

               when I
    @chord{Eb} copy them into
    @chord{Ab} Word?

In both variants, a parser should end up with the following tree:

    SONG
     |- LINE
     |   |- LBLOCK:  What is
     |   |- CLBLOCK:
     |   |   |- CBLOCK: Cm
     |   |   |- LBLOCK: up with the chords they
     |   |
     |   |- CLBLOCK:
     |       |- CBLOCK: Ab
     |       |- LBLOCK: don't align anymore
     |
     |- LINE
     |   |- LBLOCK:  when I
     |   |- CLBLOCK:
     |   |   |- CBLOCK: Eb
     |   |   |- LBLOCK: copy them into
     |   |
     |   |- CLBLOCK:
     |       |- CBLOCK: Ab
     |       |- LBLOCK: Word?

This could now get formatted as HTML, LaTeX or even as monospaced
text.

# Acknowledgements
The idea of this project was actually from a co-worker. Christian,
I hope you enjoy this mess.