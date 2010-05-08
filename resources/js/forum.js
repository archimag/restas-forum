// forum.js

$(document).ready( function () { 
    $("a.reply-link").click(showReplyForm); 
    $("input[type='reset']").click(hideReplyForm);
});

function replyForm () { return $('form'); }

function wysiwygTextArea () { return $('#wysiwyg'); }

function hideReplyForm () {
    replyForm().hide();
    var textarea = wysiwygTextArea();
    textarea.prev().remove();
    textarea.show();
}

function showReplyForm (evt) {
    var form = replyForm();

    hideReplyForm();
    form.detach()
    $(evt.target).parent().parent().after(form);
     
    var href = $(evt.target).attr("href");
    if (href && href != "") form.attr("action", href);
    
    form.show();    
    wysiwygTextArea().wysiwyg(
        { controls : 
             { separator04 : { visible : true },
               insertOrderedList : { visible : true },
               insertUnorderedList : { visible : true }
             }
        }
    );
    
    return false;
}

function newmessage () {
    var form = $('form');

    if (form.css("display") == "none") {
        form.show();

        $('#wysiwyg').wysiwyg(
            {
                controls : {
                    separator04 : { visible : true },

                    insertOrderedList : { visible : true },
                    insertUnorderedList : { visible : true }
                }
            }
        );

        location.hash = "#editor";
    }
}

