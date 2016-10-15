import React from 'react';

class App extends React.Component {
  render() {
    // Using JSX:
    return <h1>Hello World</h1>

    // Alternatively without using JSX:
    // return React.createElement('h1', null, 'Hello Guys'); // element, props, content
  }
}

// Stateless function component.
// const App = () => <h1>Hello Eggheads</h1>

export default App;
