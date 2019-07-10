// the event handler listens to shiny for messages send by handler1
// if it receives a message, call the callback function doAwesomething and pass the message
Shiny.addCustomMessageHandler("handler1", sendmessage );

// this function is called by the handler, which passes the message
function sendmessage(message){

  // show the messsage as an alert
  alert(message);
}
