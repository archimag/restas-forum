// forum.js

$(document).ready( function () { 
    $("a.reply-link").click(showReplyForm); 
    $("input[type='reset']").click(hideReplyForm);
    $("#insert-code-preview").click(previewColorizeCode);
    $("#insert-code-ok").click(insertColorizeCode);
});

function insertCodeFrame () { return $("#insert-code"); }

function replyForm () { return $('form'); }

function wysiwygTextArea () { return $('#wysiwyg'); }

function hideReplyForm () {
    replyForm().hide();
    var textarea = wysiwygTextArea();
    textarea.prev().remove();
    textarea.show();
}

function showWysiwygEditor (form) {
    form.show();    

    wysiwygTextArea().wysiwyg({
        controls : {
            insertCode: {
                visible: true,
                exec: function () {
                   insertCodeFrame().jqm();
                   insertCodeFrame().jqmShow();
            },
            tooltip: "Insert code"}
         },
         css: "http://" + location.host + "/css/style.css" });
};


function showReplyForm (evt) {
    var form = replyForm();

    hideReplyForm();
    form.detach()
    $(evt.target).parent().parent().after(form);
     
    var href = $(evt.target).attr("href");
    if (href && href != "") form.attr("action", href);

    showWysiwygEditor(form);
    
    return false;
}

function newmessage () {
    var form = $('form');

    if (form.css("display") == "none") {
        showWysiwygEditor(form)
        location.hash = "#editor";
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// Insert code dialog
////////////////////////////////////////////////////////////////////////////////////////////////////

function getColorizingCode (frame) {
    var area =  frame || insertCodeFrame();

    var lang = $("select", area).val();
    var code = $("textarea", area).val();

    if (lang == "NONE") return "<br /><pre class=\"code\">" + code + "</pre><br />";
    
    return "<br /><div class=\"code\">" +
           $.ajax( {
             url: "/forum/colorize",
             type: "POST",
             data: { lang: lang, 
                     code: code },
              async: false
           }).responseText +
           "</div><br />";
}

function previewColorizeCode (evt) {
    var frame = insertCodeFrame();
    
    var sourceArea = $("textarea", frame);
    var previewArea = sourceArea.next();

    if (previewArea.css("display") == "none") {
        previewArea.width( sourceArea.width() - 30 );
        previewArea.height( sourceArea.height() );
        previewArea.html( getColorizingCode(frame) );
 
        sourceArea.hide();
        previewArea.show();

        $("#insert-code-preview").html("Редактировать");
    }
    else {
        previewArea.hide();
        sourceArea.show();

        $("#insert-code-preview").html("Предпросмотр");
    }
}

function insertColorizeCode (evt) {
    var frame = insertCodeFrame();
    wysiwygTextArea().wysiwyg('insertHtml',
                               getColorizingCode(frame));
    frame.jqmHide();
}