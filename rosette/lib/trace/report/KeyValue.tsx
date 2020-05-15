import * as React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import TableRow from '@material-ui/core/TableRow';
import TableCell from '@material-ui/core/TableCell';

interface IKeyValueProps {
  title: string | React.ReactElement
}

const useStyles = makeStyles((theme) => ({
  key: {
    verticalAlign: 'top',
    fontWeight: 'bold',
  },
  cell: {
    border: 'none',
  }
}));

const KeyValue: React.FC<IKeyValueProps> = ({ title, children }) => {
  const classes = useStyles();
  return <TableRow>
    <TableCell component="th" scope="row" className={`${classes.key} ${classes.cell}`}>
      {title}
    </TableCell>
    <TableCell className={classes.cell}>{children}</TableCell>
  </TableRow>;
}

export default KeyValue;