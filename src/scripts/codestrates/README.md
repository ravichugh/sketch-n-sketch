==== How to make Sketch'n Sketch work on Codestrates (basic API) ====

The time of writing this document is May 11th, 2018. Codestrates might evolve as it is an experimental project.

To use a very lightweight Sketch'n Sketch version inside Codestrates, do the following:

1. Create a new codestrates (follow the [instructions on the website](https://github.com/Webstrates/Codestrates#installation))
2. Add a Section            (first button on the top-right menu)
3. click on the orange parameter "name" and enter for example "Program Viewer"
4. Add a Body Paragraph, name it "Assets"  (second button on the top-right menu)
5. Drop the file `sns.js` on it. Wait for it to upload (a dozen seconds).
   Note that the makefile ensures that the API of sns.js is exposed (lines 7 and 32 of Makefile)
   If `make all` or `make html` does not execute this command properly (e.g. `sed` is not installed), the following will not work.
6. Add a Body Paragraph, name it "Program source"
   At this point, we cannot edit the HTML of this paragraph. We need to install a package for that.
7. Install the package "Text Tools":
   1. On the top-left menu, click "CodeStrates", then "Package management", then "Install and update packages"
   2. Wait for the view to display the packages, select "Text Tools", and confirm by clicking INSTALL/UPDATE.
   After installation, reposition the cursor on the content below "Program Source"
8. On the bottom right, an icon '</>' opens the HTML editor. Click on it.
9. Paste the following content and then save it using the flappy disk icon.

    Source code of the displayed page:
    <div id="programdef">let x = 'Hello' in
    &lt;div&gt;&lt;h1&gt;@(x + ' world')&lt;/h1&gt;@x, This is a test &lt;/div&gt;</div>

10. Add a Style Paragraph, name it "Styling"       (third button on the top-right menu)
    Paste in it the content of the file `style.css`.
11. Add a Code Paragraph, name it "Initialization" (fourth button on the top-right menu)
    Paste in it the content of the file `initialization.js`
    Click on the running person. This means that this code will be executed on reloading the page.
12. Add a Code Paragraph, name it "View handlers"
    Click on the #id, and write `viewhandlers`.
    Paste in it the content of the file `viewhandler.js`
13. Add a Code Paragraph, name it "Import SNS and callbacks"
    Paste in it the content of the file `callbacks.js`
    Click on the running person for this Code Paragraph.
14. Add a Body Paragraph, name it "Preview of the program output"
    Click on the bottom `</>` to open th HTML editor, paste the following content and then save it:
    <div id="programerroroutput" class="error-hidden"></div>
    <div id="programoutput"></div>

15. Long left click on the dashed square icon of the paragraph you just created (the first one of its menu bar)
    This ensures that on loading the page, only this section is displayed.

You're done. Reload the page! On the top-right, you can alway click to see the source again.
Then, you can edit the code or the output.