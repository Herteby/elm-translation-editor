import { Elm } from './Main.elm'

const app = Elm.Main.init({
  node: document.querySelector('main')
})

app.ports.copyToClipboard.subscribe(string => {
  const textarea = document.createElement("textarea")
  textarea.textContent = string
  document.body.appendChild(textarea)
  textarea.select()
  document.execCommand("copy")
  textarea.remove()
})