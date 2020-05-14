import * as React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Fab from '@material-ui/core/Fab';
import Table from '@material-ui/core/Table';
import TableHead from '@material-ui/core/TableHead';
import TableBody from '@material-ui/core/TableBody';
import TableRow from '@material-ui/core/TableRow';
import TableCell from '@material-ui/core/TableCell';
import TableContainer from '@material-ui/core/TableContainer';
import Paper from '@material-ui/core/Paper';

import KeyboardArrowDownIcon from '@material-ui/icons/KeyboardArrowDown';
import KeyboardArrowUpIcon from '@material-ui/icons/KeyboardArrowUp';

import KeyValue from './KeyValue';

interface IStackProps {
  data: (ICallStack | IExnTrace)[],
  name: string,
}

const useStyles = makeStyles((theme) => ({
  margin: {
    display: 'block',
    marginTop: 20,
    margin: 'auto',
  },
}));

const Stacktrace: React.FC<IStackProps> = ({ data, name }) => {
  const classes = useStyles();
  const [showAll, setShowAll] = React.useState(false);
  const rows = data.length <= 5 || showAll ? data : data.slice(0, 5);
  const title = <>
    {name}
    {
      (data.length > 5)
        ? <Fab
          size="medium"
          className={classes.margin}
          onClick={() => setShowAll(!showAll)}>
          {showAll ? <KeyboardArrowUpIcon /> : <KeyboardArrowDownIcon />}
        </Fab>
        : null
    }
  </>;
  return <KeyValue title={title}>
    <TableContainer component={Paper}>
      <Table>
        <TableHead>
          <TableRow>
            <TableCell style={{ width: '50%', fontWeight: 'bold' }}>Name</TableCell>
            <TableCell style={{ width: '50%', fontWeight: 'bold' }}>Source</TableCell>
            <TableCell style={{ width: 0, fontWeight: 'bold' }}>Line</TableCell>
            <TableCell style={{ width: 0, fontWeight: 'bold' }}>Column</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {rows.map((row, i) => {
            return <TableRow key={i}>
              <TableCell><code>{row.name}</code></TableCell>
              <TableCell><code>{row.srcloc?.source}</code></TableCell>
              <TableCell>{row.srcloc?.line}</TableCell>
              <TableCell>{row.srcloc?.column}</TableCell>
            </TableRow>;
          })}
        </TableBody>
      </Table>
    </TableContainer>
  </KeyValue>;
}

export default Stacktrace;