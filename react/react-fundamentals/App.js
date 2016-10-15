import React from 'react';

class App extends React.Component {
  render() {
    /* If returning multiple elements, must wrap them in a single node.
     * The parantheses are optional, but if excluded the first tag must
     * immediately follow the return. */
    return (
      <div>
        <h1>Hello World</h1>
        <b>Bold</b>
      </div>
    );
  }
}

export default App;
