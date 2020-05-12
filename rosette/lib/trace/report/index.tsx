import * as React from 'react';
import { render } from 'react-dom';
import { makeStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import Radio from '@material-ui/core/Radio';
import FormControlLabel from '@material-ui/core/FormControlLabel';
import Toolbar from '@material-ui/core/Toolbar';
import Table from '@material-ui/core/Table';
import TableHead from '@material-ui/core/TableHead';
import TableBody from '@material-ui/core/TableBody';
import TableRow from '@material-ui/core/TableRow';
import TableCell from '@material-ui/core/TableCell';
import TableContainer from '@material-ui/core/TableContainer';
import Paper from '@material-ui/core/Paper';
import Fab from '@material-ui/core/Fab';

import CircularProgress from '@material-ui/core/CircularProgress';
import IconButton from '@material-ui/core/IconButton';
import KeyboardArrowDownIcon from '@material-ui/icons/KeyboardArrowDown';
import KeyboardArrowUpIcon from '@material-ui/icons/KeyboardArrowUp';

import lightBlue from '@material-ui/core/colors/lightBlue';

import KeyValue from './KeyValue';
import EnhancedTable from './EnhancedTable';

import { getFirstLine } from './utils';

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
  margin: {
    display: 'block',
    marginTop: '20px',
    margin: 'auto',
  },
  spacer: {
    flex: '1 1 10%'
  },
}));

let ws: WebSocket | null = null;
let cnt: number = 0;

const makeGroup = (trace: ITraceEntrySingle[]) => {
  // Map in JS couldn't deal with structural equality of keys, so 
  // stringify them first :(

  const mapper = new Map<string, ITraceEntryGroup>();

  for (const { exn_msg, stx_info, timestamp, exn_trace, call_stack, key } of trace) {
    const trimmedExnMsg = getFirstLine(exn_msg)
    const groupKey = JSON.stringify({
      exnMsg: trimmedExnMsg,
      stx_info
    });
    const group: ITraceEntryGroup = mapper.get(groupKey) || {
      kind: 'group',
      key: groupKey,
      stx_info,
      exn_msg: trimmedExnMsg,
      group: [],
    };
    group.group.push({
      key,
      timestamp,
      exn_trace,
      call_stack,
      full_exn_msg: exn_msg
    });
    mapper.set(groupKey, group);
  }

  const arr = Array.from(mapper.values());
  arr.sort((x: ITraceEntryGroup, y: ITraceEntryGroup) =>
    x.exn_msg.localeCompare(y.exn_msg)
  );
  return arr;
}

interface IStackProps {
  data: (ICallStack | IExnTrace)[],
  name: string,
}

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
          <IconButton size="small">
            {showAll ? <KeyboardArrowUpIcon /> : <KeyboardArrowDownIcon />}
          </IconButton>
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

const handleSubDetailPanel = (row: IGroupDetail) => {
  const classes = useStyles();
  return <TableContainer component={Paper}>
    <Table>
      <colgroup>
        <col />
        <col className={classes.full_width} />
      </colgroup>
      <TableBody>
        <KeyValue title="Error">
          <div className={classes.pre}>{row.full_exn_msg}</div>
        </KeyValue>
        <Stacktrace name="Rosette stacktrace" data={row.call_stack} />
        <Stacktrace name="Racket stacktrace" data={row.exn_trace} />
      </TableBody>
    </Table>
  </TableContainer>;
}

const handleDetailPanel = (row: ITraceEntry) => {
  const classes = useStyles();

  const renderRest = () => {
    switch (row.kind) {
      case 'no-group': return <>
        <KeyValue title="Created at">
          {new Date(row.timestamp * 1000).toUTCString()}
        </KeyValue>
        <KeyValue title="Error">
          <div className={classes.pre}>{row.exn_msg}</div>
        </KeyValue>
        <Stacktrace name="Rosette stacktrace" data={row.call_stack} />
        <Stacktrace name="Racket stacktrace" data={row.exn_trace} />
      </>;
      case 'group': return <TableRow>
        <TableCell colSpan={2}>
          <EnhancedTable<IGroupDetail>
            data={row.group}
            initialNumRows={5}
            onDetailPanel={handleSubDetailPanel}
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
          <div className={classes.pre}>{row.stx_info?.stx}</div>
        </KeyValue>
        {renderRest()}
      </TableBody>
    </Table>
  </TableContainer>;
};

const App: React.FC = () => {
  const classes = useStyles();
  const [trace, setTrace] = React.useState<ITraceEntrySingle[]>([]);
  const [title, setTitle] = React.useState('');
  const [mode, setMode] = React.useState<'group' | 'no-group'>('group');
  const [isLoading, setLoading] = React.useState(true);
  React.useEffect(() => {
    const urlParams = new URLSearchParams(window.location.search);
    setTitle(urlParams.get('title') || 'Untitled');
    ws = new WebSocket('ws://localhost:' + (urlParams.get('port') || '8048'));
    return () => {
      ws?.close();
    };
  }, []); // ensure this useEffect is invoked only once

  React.useEffect(() => {
    if (ws) {
      ws.onmessage = (evt: MessageEvent) => {
        const parsed: IEntry[] = JSON.parse(evt.data);
        const newTrace: ITraceEntrySingle[] = [];
        for (const entry of parsed) {
          switch (entry.type) {
            case 'trace':
              newTrace.push({
                kind: 'no-group',
                // it's fine to use autoincrement key here because 
                // elements won't get deleted or moved
                key: (cnt++).toString(),
                ...entry.data
              });
              break;
            case 'stats': setLoading(false); break;
            case 'shutdown': break;
            default: throw new Error("infeasible");
          }
        }
        setTrace(trace => trace.concat(newTrace));
      };
    }
  });

  const handleModeChange = (evt: React.ChangeEvent<HTMLInputElement>) => {
    setMode(evt.target.value as any);
  };

  const data = mode === 'no-group' ? trace : makeGroup(trace);

  const topBar = (
    <Grid container spacing={6} alignItems="center">
      <Grid item>
        <h1><code>{title}</code></h1>
      </Grid>
      <Grid item>
        <FormControlLabel
          control={
            <Radio
              checked={mode === 'group'}
              onChange={handleModeChange}
              value="group"
              size="small"
            />
          }
          label="Group"
          labelPlacement="end"
        />
        <FormControlLabel
          control={
            <Radio
              checked={mode === 'no-group'}
              onChange={handleModeChange}
              value="no-group"
              size="small"
            />
          }
          label="No group"
          labelPlacement="end"
        />
      </Grid>
    </Grid>
  );

  return <>
    <Toolbar>
      {topBar}
      <div className={classes.spacer} />
      {isLoading ? <CircularProgress /> : null}
    </Toolbar>
    <EnhancedTable<ITraceEntry>
      data={data}
      onDetailPanel={handleDetailPanel}
      initialNumRows={10}
      columns={[
        {
          title: 'Error',
          width: '50%',
          render: (row: ITraceEntry) => <code>{getFirstLine(row.exn_msg)}</code>,
          align: 'left',
        },
        {
          title: 'Source',
          width: '50%',
          render: (row: ITraceEntry) => <code>{row.stx_info?.srcloc.source}</code>,
          align: 'left',
        },
        {
          title: 'Line',
          width: 0,
          render: (row: ITraceEntry) => <>{row.stx_info?.srcloc.line}</>,
          align: 'right',
        },
        {
          title: 'Column',
          width: 0,
          render: (row: ITraceEntry) => <>{row.stx_info?.srcloc.column}</>,
          align: 'right',
        },
      ]}
    />
  </>;
}

render(<App />, document.getElementById("root"));

if (module.hot) {
  module.hot.accept();
}