import Component from '@glimmer/component';
import { action } from '@ember/object';
import { tracked } from '@glimmer/tracking';

import {
  run, runLots, add, update, swapRows, deleteRow,
} from 'ember-temp/utils/benchmark-helpers';

export default class MyTable extends Component {
  @tracked
  id = 1;
  @tracked
  data = [];
  @tracked
  selected = undefined;

  @action create() {
    const result = run(this.id);

    this.id = result.id;
    this.data = result.data;
    this.selected  = undefined;
  }

  @action add() {
    this.data = add(this.id, this.data)
  }

  @action update() {
    update(this.data);
  }

  @action runLots() {
    const result = runLots(this.id);

    this.data = result.data;
    this.id = result.id;
    this.selected = undefined;
  }

  @action clear() {
    this.data = [];
    this.selected  = undefined;
  }

  @action swapRows() {
    this.data = swapRows(this.data);
  }

  @action remove(id) {
    const selected = this.data.find(({selected}) => selected === true);
    if (selected) {
      selected.selected = false;
    }
    this.data = deleteRow(this.data, id);
    this.selected = undefined;
  }

  @action select(id) {
    this.selected = id;
    const selected = this.data.find(({selected}) => selected === true);
    if (selected) {
      selected.selected = false;
    }
    this.data.find((item)=>item.id === id).selected = true;
  }
}
