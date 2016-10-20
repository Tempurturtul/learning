import React from 'react';

class App extends React.Component {
  constructor() {
    super();
    this.state = {
      data: [
        {id: 1, name: "Simon Bailey"},
        {id: 2, name: "Thomas Buleson"},
        {id: 3, name: "Will Button"},
        {id: 4, name: "Ben Clinkinbeard"}
      ]
    };
  }

  render() {
    let rows = this.state.data.map(person => {
      // Siblings require unique keys.
      return <PersonRow key={person.id} data={person} />;
    });

    return (
      <table>
        <tbody>{rows}</tbody>
      </table>
    );
  }
}

const PersonRow = (props) => {
  return (
    <tr>
      <td>{props.data.id}</td>
      <td>{props.data.name}</td>
    </tr>
  );
}

export default App;
