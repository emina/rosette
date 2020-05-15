import * as React from 'react';
import { render } from 'react-dom';

import { makeStyles } from '@material-ui/core/styles';

import Grid from '@material-ui/core/Grid';
import FormControlLabel from '@material-ui/core/FormControlLabel';
import Toolbar from '@material-ui/core/Toolbar';
import Switch from '@material-ui/core/Switch';
import TextField from '@material-ui/core/TextField';
import InputAdornment from '@material-ui/core/InputAdornment';
import CircularProgress from '@material-ui/core/CircularProgress';
import IconButton from '@material-ui/core/IconButton';

import ClearIcon from '@material-ui/icons/Clear';
import SearchIcon from '@material-ui/icons/Search';

import { debounce } from 'debounce';

import EnhancedTable from './EnhancedTable';
import DetailPanel from './DetailPanel';

import { ShowRacketContext } from './context';

const DEBOUNCE = 200;

const useStyles = makeStyles((theme) => ({
  spacer: {
    flex: '1 1 10%'
  },
  progress: {
    marginLeft: 20,
  },
  tool: {
    marginLeft: 0,
    marginRight: theme.spacing(2),
  }
}));

let ws: WebSocket | null = null;
let cnt: number = 0;

const makeGroup = (trace: ITraceEntrySingle[]) => {
  // Map in JS couldn't deal with structural equality of keys, so 
  // stringify them first

  const mapper = new Map<string, ITraceEntryGroup>();

  for (const { exnMsg, stxInfo, timestamp, exnTrace, callStack, key } of trace) {
    const trimmedExnMsg = getFirstLine(exnMsg)
    const groupKey = JSON.stringify({
      exnMsg: trimmedExnMsg,
      stxInfo
    });
    const group: ITraceEntryGroup = mapper.get(groupKey) || {
      kind: 'group',
      key: groupKey,
      stxInfo,
      exnMsg: trimmedExnMsg,
      group: [],
    };
    group.group.push({
      key,
      timestamp,
      exnTrace,
      callStack,
      fullExnMsg: exnMsg
    });
    mapper.set(groupKey, group);
  }

  const arr = Array.from(mapper.values());
  arr.sort((x: ITraceEntryGroup, y: ITraceEntryGroup) =>
    x.exnMsg.localeCompare(y.exnMsg)
  );
  return arr;
}

const filter = (trace: ITraceEntrySingle[], query: string) =>
  query === '' ? trace : trace.filter(e => e.exnMsg.includes(query));

const getFirstLine = (exnMsg: string) => exnMsg.split('\n')[0];

const App: React.FC = () => {
  const classes = useStyles();

  const [trace, setTrace] = React.useState<ITraceEntrySingle[]>([]);
  const [title, setTitle] = React.useState('');
  const [isGrouped, setIsGrouped] = React.useState(true);
  const [showRacket, setShowRacket] = React.useState(false);
  const [isLoading, setLoading] = React.useState(true);

  // NOTE: query and searchText are technically the same, but 
  // one will be updated immediately and displayed in the textbox
  // while the other's update will be debounced for search performance
  const [searchText, setSearchText] = React.useState('');
  const [query, setQuery] = React.useState('');
  // useMemo here so that debounce doesn't create a new decounceSetQuery
  // every time it's rendered.
  const debounceSetQuery = React.useMemo(() => debounce(setQuery, DEBOUNCE), []);

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
            default: throw new Error('infeasible');
          }
        }
        // Here's an optimization to preserve object identitiy
        setTrace(trace => newTrace.length === 0
          ? trace
          : trace.concat(newTrace));
      };
    }
  });

  const handleIsGroupedChange = (evt: React.ChangeEvent<HTMLInputElement>) => {
    setIsGrouped(evt.target.checked);
  }

  const handleShowRacketChange = (evt: React.ChangeEvent<HTMLInputElement>) => {
    setShowRacket(evt.target.checked);
  }

  const handleSearchChange = (evt: React.ChangeEvent<HTMLInputElement>) => {
    setSearchText(evt.target.value);
    debounceSetQuery(evt.target.value);
  }

  const handleClearSearch = () => {
    setSearchText('');
    debounceSetQuery('');
  }

  // useMemo here so that given the same trace and query, 
  // no computation is needed and object identitiy is preserved.
  const filteredTrace =
    React.useMemo(() => filter(trace, query), [trace, query]);

  // useMemo here so that given the same isGrouped and filteredTrace,
  // no computation is needed and object identitiy is preserved.
  const data = React.useMemo(() => isGrouped
    ? makeGroup(filteredTrace)
    : filteredTrace,
    [filteredTrace, isGrouped]);

  const topBar = (
    <Grid container alignItems="center">
      <Grid item xs={12} md={3}>
        <h1>
          <code>{title}</code>
          {isLoading
            ? <CircularProgress
              size={20}
              disableShrink
              className={classes.progress} />
            : null}
        </h1>
      </Grid>
      <Grid item xs={12} md={3}>
        <FormControlLabel
          value="top"
          control={
            <Switch
              checked={isGrouped}
              onChange={handleIsGroupedChange}
            />
          }
          className={classes.tool}
          label="Group similar rows"
          labelPlacement="start"
        />
      </Grid>
      <Grid item xs={12} md={3}>
        <FormControlLabel
          value="top"
          control={
            <Switch
              checked={showRacket}
              onChange={handleShowRacketChange}
            />
          }
          className={classes.tool}
          label="Show Racket stacktrace"
          labelPlacement="start"
        />
      </Grid>
    </Grid>
  );

  return <>
    <Toolbar>
      {topBar}
      <TextField
        value={searchText}
        onChange={handleSearchChange}
        placeholder="Search"
        InputProps={{
          startAdornment: <InputAdornment position="start">
            <SearchIcon fontSize="small" />
          </InputAdornment>,
          endAdornment: <InputAdornment position="end">
            <IconButton onClick={handleClearSearch}>
              <ClearIcon color="inherit" fontSize="small" />
            </IconButton>
          </InputAdornment>,
        }}
      />
    </Toolbar>
    <ShowRacketContext.Provider value={showRacket}>
      <EnhancedTable<ITraceEntry>
        data={data}
        detailPanel={DetailPanel}
        initialNumRows={10}
        columns={[
          {
            title: 'Error',
            width: '70%',
            render: (row: ITraceEntry) =>
              <code>{getFirstLine(row.exnMsg)}</code>,
            align: 'left',
          },
          {
            title: 'Source',
            width: '30%',
            render: (row: ITraceEntry) =>
              <code>{row.stxInfo?.srcloc.source}</code>,
            align: 'left',
          },
          {
            title: 'Line',
            width: 0,
            render: (row: ITraceEntry) => <>{row.stxInfo?.srcloc.line}</>,
            align: 'right',
          },
          {
            title: 'Column',
            width: 0,
            render: (row: ITraceEntry) => <>{row.stxInfo?.srcloc.column}</>,
            align: 'right',
          },
        ]}
      />
    </ShowRacketContext.Provider>
  </>;
}

render(<App />, document.getElementById('root'));

if (module.hot) {
  module.hot.accept();
}