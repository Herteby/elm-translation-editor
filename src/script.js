import { Elm } from './Main.elm'

const app = Elm.Main.init({
  node: document.querySelector('main')
})

app.ports.confirmBack.subscribe(()=> {
  if(confirm("Are you sure you want to go back? Your work will be lost." )){
    app.ports.confirmedBack.send(null)
  }
})

app.ports.copyToClipboard.subscribe(string => {
  const textarea = document.createElement("textarea")
  textarea.textContent = string
  document.body.appendChild(textarea)
  textarea.select()
  document.execCommand("copy")
  textarea.remove()
})