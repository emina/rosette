import * as React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Table from '@material-ui/core/Table';
import TableBody from '@material-ui/core/TableBody';
import TableRow from '@material-ui/core/TableRow';
import TableCell from '@material-ui/core/TableCell';
import TableContainer from '@material-ui/core/TableContainer';
import Paper from '@material-ui/core/Paper';

import lightBlue from '@material-ui/core/colors/lightBlue';

import KeyValue from './KeyValue';
import EnhancedTable from './EnhancedTable';
import Stacktrace from './Stacktrace';

import { ShowRacketContext } from './context';

const useStyles = makeStyles((theme) => ({
  pre: {
    fontFamily: 'monospace',
    whiteSpace: 'pre-wrap',
  },
  full_width: {
    width: '100%',
  },
  details: {
    backgroundColor: lightBlue[50],
  },
}));

const SubDetailPanel: React.FC<Row<IGroupDetail>> = ({ row }) => {
  const classes = useStyles();
  return <TableContainer component={Paper}>
    <Table>
      <colgroup>
        <col />
        <col className={classes.full_width} />
      </colgroup>
      <TableBody>
        <KeyValue title="Error">
          <div className={classes.pre}>{row.fullExnMsg}</div>
        </KeyValue>
        <Stacktrace name="Rosette stacktrace" data={row.callStack} />
        <ShowRacketContext.Consumer>
          {value => value
            ? <Stacktrace name="Racket stacktrace" data={row.exnTrace} />
            : null}
        </ShowRacketContext.Consumer>
      </TableBody>
    </Table>
  </TableContainer>;
}

const DetailPanel: React.FC<Row<ITraceEntry>> = ({ row }) => {
  const classes = useStyles();

  const renderRest = () => {
    switch (row.kind) {
      case 'no-group': return <>
        <KeyValue title="Created at">
          {new Date(row.timestamp * 1000).toUTCString()}
        </KeyValue>
        <KeyValue title="Error">
          <div className={classes.pre}>{row.exnMsg}</div>
        </KeyValue>
        <Stacktrace name="Rosette stacktrace" data={row.callStack} />
        <ShowRacketContext.Consumer>
          {value => value
            ? <Stacktrace name="Racket stacktrace" data={row.exnTrace} />
            : null}
        </ShowRacketContext.Consumer>
      </>;
      case 'group': return <TableRow>
        <TableCell colSpan={2}>
          <EnhancedTable<IGroupDetail>
            data={row.group}
            initialNumRows={5}
            detailPanel={SubDetailPanel}
            columns={[
              {
                title: 'Created at',
                render: (row: IGroupDetail) =>
                  <>{new Date(row.timestamp * 1000).toUTCString()}</>,
                align: 'left',
                width: '100%',
              },
            ]}
          />
        </TableCell>
      </TableRow>;
    }
  };
  return <TableContainer component={Paper}>
    <Table className={classes.details}>
      <colgroup>
        <col />
        <col className={classes.full_width} />
      </colgroup>
      <TableBody>
        <KeyValue title="Blame">
          <div className={classes.pre}>{row.stxInfo?.stx}</div>
        </KeyValue>
        {renderRest()}
      </TableBody>
    </Table>
  </TableContainer>;
};

export default DetailPanel;