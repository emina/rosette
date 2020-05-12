import * as React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Collapse from '@material-ui/core/Collapse';
import Table from '@material-ui/core/Table';
import TableHead from '@material-ui/core/TableHead';
import TableBody from '@material-ui/core/TableBody';
import TableCell from '@material-ui/core/TableCell';
import TableFooter from '@material-ui/core/TableFooter';
import TablePagination from '@material-ui/core/TablePagination';
import TableRow from '@material-ui/core/TableRow';
import Typography from '@material-ui/core/Typography';

import IconButton from '@material-ui/core/IconButton';
import FirstPageIcon from '@material-ui/icons/FirstPage';
import KeyboardArrowLeft from '@material-ui/icons/KeyboardArrowLeft';
import KeyboardArrowRight from '@material-ui/icons/KeyboardArrowRight';
import LastPageIcon from '@material-ui/icons/LastPage';
import KeyboardArrowDownIcon from '@material-ui/icons/KeyboardArrowDown';
import KeyboardArrowUpIcon from '@material-ui/icons/KeyboardArrowUp';

const useStylesPagination = makeStyles(theme => ({
  root: {
    flexShrink: 0,
    marginLeft: theme.spacing(2.5),
  },
  label: {
    flex: 1,
    textAlign: 'center',
    alignSelf: 'center',
    flexBasis: 'inherit',
  },
}));

interface IPaginationActionsProps {
  count: number,
  page: number,
  rowsPerPage: number,
  onChangePage: (
    event: React.MouseEvent<HTMLButtonElement, MouseEvent> | null,
    page: number
  ) => void,
}

const PaginationActions: React.FC<IPaginationActionsProps> = (
  { count, page, rowsPerPage, onChangePage }
) => {
  const classes = useStylesPagination();

  const handleFirstPageButtonClick = (evt) => {
    onChangePage(evt, 0);
  };

  const handleBackButtonClick = (evt) => {
    onChangePage(evt, page - 1);
  };

  const handleNextButtonClick = (evt) => {
    onChangePage(evt, page + 1);
  };

  const handleLastPageButtonClick = (evt) => {
    onChangePage(evt, Math.max(0, Math.ceil(count / rowsPerPage) - 1));
  };

  const from = page * rowsPerPage + 1;
  const to = Math.min((page + 1) * rowsPerPage, count);

  return (
    <div className={classes.root}>
      <IconButton
        onClick={handleFirstPageButtonClick}
        disabled={page === 0}>
        <FirstPageIcon />
      </IconButton>
      <IconButton onClick={handleBackButtonClick} disabled={page === 0}>
        <KeyboardArrowLeft />
      </IconButton>
      <Typography variant="caption" className={classes.label}>
        {`${from}-${to} of ${count}`}
      </Typography>
      <IconButton
        onClick={handleNextButtonClick}
        disabled={page >= Math.ceil(count / rowsPerPage) - 1}>
        <KeyboardArrowRight />
      </IconButton>
      <IconButton
        onClick={handleLastPageButtonClick}
        disabled={page >= Math.ceil(count / rowsPerPage) - 1}>
        <LastPageIcon />
      </IconButton>
    </div>
  );
};

interface IRowProps<T> {
  row: T,
  onDetailPanel?: (row: T) => React.ReactNode,
  colDefinitions: Column<T>[],
}

const Row = <T,>({ row, onDetailPanel, colDefinitions }: IRowProps<T>) => {
  const [open, setOpen] = React.useState(false);

  const rowElement: React.ReactNode[] = colDefinitions.map((col, i) => {
    return <TableCell key={i} style={{ width: col.width }} align={col.align}>
      {col.render(row)}
    </TableCell>;
  });

  if (onDetailPanel) {
    rowElement.unshift(
      <TableCell key="switch" style={{ width: 0 }} >
        <IconButton size="small" onClick={() => setOpen(!open)}>
          {open ? <KeyboardArrowUpIcon /> : <KeyboardArrowDownIcon />}
        </IconButton>
      </TableCell>
    );
  }

  return <>
    <TableRow>{...rowElement}</TableRow>
    {onDetailPanel && <TableRow>
      <TableCell style={{ padding: 0 }} colSpan={6}>
        <Collapse in={open} timeout="auto" unmountOnExit>
          {onDetailPanel(row)}
        </Collapse>
      </TableCell>
    </TableRow>}
  </>;
}

const useStylesMain = makeStyles({
  table: { minWidth: 500 },
  paginationCaption: { display: 'none' },
  paginationSelectRoot: { margin: 0 },
  tableHead: { fontWeight: 'bold' },
});

interface IEnhancedTableProps<RowData> {
  data: RowData[],
  onDetailPanel?: (rowData: RowData) => React.ReactNode,
  columns: Column<RowData>[],
  initialNumRows?: number,
}

interface Column<RowData> {
  title: string,
  width?: number | string,
  render: (data: RowData) => React.ReactNode,
  align: ('left' | 'right'),
}

interface IHasKeyProps {
  key: string,
}

const EnhancedTable = <T extends IHasKeyProps>({ data, columns, onDetailPanel, initialNumRows }: IEnhancedTableProps<T>) => {
  const classes = useStylesMain();
  const [page, setPage] = React.useState(0);
  const [rowsPerPage, setRowsPerPage] = React.useState(initialNumRows || 10);

  const emptyRows = rowsPerPage - Math.min(rowsPerPage, data.length - page * rowsPerPage);

  const handleChangePage = (evt, newPage) => {
    setPage(newPage);
  };

  const handleChangeRowsPerPage = (evt) => {
    setRowsPerPage(parseInt(evt.target.value, 10));
    setPage(0);
  };

  const headers: React.ReactNode[] = columns.map((col, i) => <TableCell
    key={i}
    style={{ width: col.width, fontWeight: 'bold' }}
    align={col.align}>
    {col.title}
  </TableCell>);

  if (onDetailPanel) {
    headers.unshift(<TableCell key="switch" style={{ width: 0 }} />);
  }

  return (
    <>
      <div style={{ overflowX: 'auto', position: 'relative' }}>
        <Table className={classes.table}>
          <TableHead>
            <TableRow>{...headers}</TableRow>
          </TableHead>
          <TableBody>
            {
              data.slice(page * rowsPerPage, page * rowsPerPage + rowsPerPage)
                .map((row: T) => (
                  <Row key={row.key} row={row} colDefinitions={columns} onDetailPanel={onDetailPanel} />
                ))
            }
            {emptyRows > 0 && (
              <TableRow style={{ height: 53 * emptyRows }}>
                <TableCell colSpan={6} />
              </TableRow>
            )}
          </TableBody>
        </Table>
      </div>
      <Table>
        <TableFooter>
          <TableRow>
            <TablePagination
              classes={{
                caption: classes.paginationCaption,
                selectRoot: classes.paginationSelectRoot,
              }}
              rowsPerPageOptions={[5, 10, 25]}
              colSpan={3}
              count={data.length}
              rowsPerPage={rowsPerPage}
              page={page}
              SelectProps={{
                renderValue: value => <div style={{ padding: '0px 5px' }}>{value + ' rows '}</div>
              }}
              onChangePage={handleChangePage}
              onChangeRowsPerPage={handleChangeRowsPerPage}
              ActionsComponent={PaginationActions}
              labelRowsPerPage=""
              labelDisplayedRows={() => ""}
            />
          </TableRow>
        </TableFooter>
      </Table>
    </>
  );
}
export default EnhancedTable;