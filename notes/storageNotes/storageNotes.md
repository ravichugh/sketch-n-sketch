#Local Storage

Specification:
* Saves 'Sketch' code input on an ongoing basis to be recovered in case of crashing
* Allow uploading of .little files to be viewed and edited
* Allow exporting of .little files to be saved and shared
* Allow expoering of .svg files to be saved and shared
* Save configuration preferences for preference persistence across sessions

##Crash Resistance/Configuration Persistence (saving work to the browser)
This can be accomplished using the browser localStorage, which is a persistent data store that is likely to persist across page crashes. Beyond mirroring the input code, it could also work as a nice 'working slate' for multiple *Scratch* files, if so desired. This would become more significant as uploading is supported, as someone might upload a file only to look at it and not want it to overwrite their only one editing window.

Look at the elmReference/Reference.elm for an annotated example where I got simple key/value storage and retrieval working.

It seems that the biggest change will be the addition of a few new events, namely:
* "Save was Successful" : User requested save or an autosave occurred, and a small display message (in the debug portion, probably) should be displayed and state parameters adjusted as appropriate.
* "Retrieval was Successful" : User requested to retrieve a file from local storage, and it should be made the active working document
* Ancillary "Save requested", "Retrieval requested", "S/R failed" events to adjust the state and display appropriate messages, if desired

Further, there will be the addition of a new mailbox and a new port - a taskMailbox to recieve the save/retrieve tasks, and the port that is mentioned in Reference.elm as being nececcary. 

Code/Page Layout saving implemented; only supports one save file. Works during
and between browser sessions.

We don't want slate saving, but there were some layout changes that we discussed. The dropdown menu is an implicit 'load' selection, so the inclusion of a separate load button is not necessary. Instead, the load button will become a 'revert' button that reverts the contents of the code box to the last save. Note that all revert functionality should also put the state back into where there is a 'run code' option available, as the slate is not rerendered (the code that they wrote might cause a crash, the possiblity of which is why they saved). Also, we need a method of saving multiple files to the browser. This will necessitate the introduction of a pop-up dialog that asks the user to provide a save name whenever they are saving a file that has not been saved before or are saving changers to an example.

Multiple saving functionality has been implemented. Some strangeness with
chrome vs. firefox.

**Style pop up box more nicely, see if highlighting can be allowed. Somehow load
all past local browser saves upon startup.**

##Saving to Disk
It looks like the HTML5 fileReader object is the modern and 'correct' way to do this. It looks like there is a library https://github.com/piotrcyr/elm-filereader that managed to implement this in 2014 without using any ports, which would be desirable. However, it doesn't look like it's been updated to 0.15, and the level of completion of the library is in question. 

The library is very clearly out of date with the current paradigm of input methods sending messages to mailboxes. I've looked into potential methods for adding an 'onchange' event handler by using the usual avenues, but there seems to be two issues:

* `on "change" decoder ...` - Does not register the appropriate `onchange="..."` attribute as one might think it would. Nothing comes out from the following code
```elm
decodeFileList : Decoder FileList
decodeFileList =
    list 
      <| object1 File
            ("name" := string)

view : Model -> Html
view model = div []
    [ input 
        [ type' "file"
        , on "change" decodeFileList <| \fl ->
            Signal.message eventMailbox.address <|  NewList fl
        ] []
    , text (fileListtoStr model.fileList)
    ]
```
Attempts at adding `at ["target", "value"] ...` to the decoder are ineffectual, and let to a broader realization...
* What really needs to be referenced is `this.files`, which is not a list but a list-like object that is called a FileList. It can be accessed in the same way as any other list, but I'm not sure if Elm can just know to try using the typical `List` accessor on objects that of type `List` in the Elm code. Further, I don't think there's any way for Elm to just 'know' that it is `this.files` that needs to be accessed, which I think is the case for other sorts of input forms (an event handler to handle the specific vagaries of that DOM element being registered).

In any case, looking at this:
https://developer.mozilla.org/en-US/docs/Using_files_from_web_applications#Dynamically_adding_a_change_listener

It seems that any dynamically added event listener (which is what the elm-runtime does 
```function addListener(relevantInputs, domNode, eventName, func)
            {
                domNode.addEventListener(eventName, func);
                var listener = {
                    relevantInputs: relevantInputs,
                    domNode: domNode,
                    eventName: eventName,
                    func: func
                };
                listeners.push(listener);
            })```) must explicitly reference the files attribute of the returned
DOM object, e.g. using `this.files`, which Elm has no way of knowing or automatically generating by the looks of it, as the existing JSON.decode will operate on JSON objects, but not on DOM nodes. So, I think a Native library will be needed to address this, and that's a can of worms that I've been slowly working my way through anyways. Somewhat frustrating, but necessary if we're going to allow downloading/uploading of files (which is a massive boon to usability).

**Let Ravi know about the writing of a simple Native library, and think of sensible Graphics.Input-like abstraction to handle reading of files. Also figure out how FileReader is meant to work**
