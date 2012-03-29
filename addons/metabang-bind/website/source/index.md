{include resources/header.md}
{set-property title "metabang-bind - Sticking it the to metal..."}

<div class="contents">
<div class="system-links">

  * [Mailing Lists][3]
  * [Getting it][4]
  * [Documentation][5]
  * [News][6]
  * [Test results][tr]
  * [Changelog][7]

   [3]: #mailing-lists
   [4]: #downloads
   [5]: documentation/ (documentation link)
   [6]: #news
   [7]: changelog.html


</div>
<div class="system-description">

### What it is

Bind combines _let_, _destructuring-bind_ and _multiple-value-bind_ and a whole lot more into a single form. The [user guide][user-guide] has all the details but here is example to whet your appetite:
    
    (bind ((a 2)
           ((b &rest args &key (c 2) &allow-other-keys) '(:a :c 5 :d 10 :e 54))
           ((:values d e) (truncate 4.5)))
      (list a b c d e args))
    ==> (2 :A 5 4 0.5 (:C 5 :D 10 :E 54))

Bind is especially handy when you have more than one layer of multiple-value-bind or destructuring-bind. Since bind is a single form, you don't end up too far off to the right in editor land.

Bind is released under the MIT license.

{anchor mailing-lists}

### Mailing Lists

Use the developer [mailing list][metabang-bind-devel] for any questions or comments regarding bind.

{anchor downloads}

### Where is it

A [Darcs][] repository is available. The command to get bind is:

    darcs get http://common-lisp.net/project/metabang-bind/

metabang-bind is also [ASDF installable][10]. Its CLiki home is right [where][11] you'd expect.

   [10]: http://www.cliki.net/asdf-install
   [11]: http://www.cliki.net/bind

There's also a handy [gzipped tar file][12].

   [12]: http://common-lisp.net/project/cl-containers/metabang-bind/metabang-bind_latest.tar.gz

{anchor news}

### What is happening

1 Dec 2007 - Added support for [array destructuring][array-bindings] (Thanks to Tamas Papp for the idea)

15 Nov 2007 - New user guide; bind handles structures and property lists and is now extensible!

13 Nov 2005 - Initial webpage n' stuff.

</div>
</div>

{include resources/footer.md}


